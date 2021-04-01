package tesic

import spinal.core.fiber.Handle
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.bmb.{BmbInterconnectGenerator, BmbToApb3Generator}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.jtag.JtagState.{DR_CAPTURE, DR_EXIT1, DR_EXIT2, DR_SHIFT}
import spinal.lib.com.jtag.{Jtag, JtagFsm, JtagTap, JtagTapInstructionCtrl, JtagTapInstructionRead, JtagTapInstructionWrite}

import scala.util.Random


class JtagTapInstructionBypass[T <: Data] extends Area {
  val ctrl = JtagTapInstructionCtrl()
  ctrl.tdo := False
  when(ctrl.enable) {
    when(ctrl.shift) {
      ctrl.tdo := ctrl.tdi
    }
  }

}

class JtagTapInstructionH2t(data: Stream[UInt]) extends Area {
  val ctrl = JtagTapInstructionCtrl()
  val inStream = Stream(UInt(widthOf(data.payload) bit))
  val shifter = Reg(Bits(widthOf(data.payload) bit))
  //will count until all bits are received which will start a push in the fifo
  val counter = Reg(UInt(log2Up(widthOf(data.payload)) + 1 bit))
  when(ctrl.enable) {
    when(ctrl.capture) {
      // The v bit is connected to fifo.push.ready byte, no Dr is accepted if we can't write in the stream
      shifter := (shifter ## data.ready.asBits(1 bit)).resize(widthOf(data.payload) bit)
    }
    when(ctrl.shift) {
      shifter := (ctrl.tdi ## shifter) >> 1
      counter := counter + 1
    } otherwise {
      //Counter should  always be equal to zero unless we are in a DR SHIFT
      counter := 0
    }
  }
  ctrl.tdo := shifter.lsb
  data << inStream
  inStream.payload.assignFromBits(shifter)
  //our data is valid only when we receive all of the bits, happens when counter ===....
  inStream.valid := (counter === widthOf(data.payload))
}

class JtagTapInstructionT2h(data: Stream[UInt]) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val shifter = Reg(Bits(widthOf(data.payload) + 1 bit))
  //used to store poped data during a peek
  val buffer = Reg(Bits(widthOf(data.payload) + 1 bit))
  val validDrShift = RegNext(ctrl.capture && ctrl.enable)
  val counter = Reg(UInt(1 bit))
  // we don't want to pop anything from the fifo unless we need it
  val lastStateIsPeek, thisStateIsPeek = Reg(Bool).init(False)
  //lastStateIsPeek:=False
  //thisStateIsPeek:=False
  data.ready := False
  when(ctrl.enable) {
    when(ctrl.capture) {
      //forcing a pop when the last peek had an lsb =0
      when(lastStateIsPeek & buffer.lsb) {
        shifter := buffer
      } otherwise {
        data.ready := True
        buffer := B(data.payload) ## data.valid
        shifter := B(data.payload) ## data.valid
      }
    }
    when(ctrl.shift) {
      counter := counter + 1
      shifter := (ctrl.tdi ## shifter) >> 1
      when(counter === 0) {
        thisStateIsPeek := ctrl.tdi
        lastStateIsPeek := thisStateIsPeek
      }
    } otherwise {
      counter := 0
    }
  }

  ctrl.tdo := shifter.lsb
}

class JtagTapTesic(jtag: Jtag, instructionWidth: Int) extends JtagTap(jtag, instructionWidth) {

  def bypass(light: Boolean = false)(instructionId: Int) = {
    val area = new JtagTapInstructionBypass()
    map(area.ctrl, instructionId)
    area
  }

  def h2t(data: Stream[UInt])(instructionId: Int) = {
    val area = new JtagTapInstructionH2t(data)
    map(area.ctrl, instructionId)
    area
  }

  def t2h(data: Stream[UInt])(instructionId: Int) = {
    val area = new JtagTapInstructionT2h(data)
    map(area.ctrl, instructionId)
    area
  }
}

case class SSTAP() extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val h2tPushStream = master Stream (UInt(32 bits))
    val t2hPopStream = slave Stream (UInt(32 bits))
  }
  val tap = new JtagTapTesic(io.jtag, 4)
  val idcodeArea = tap.idcode(B"x0BA0BAAB")(1)
  val bypassarea = tap.bypass()(2)
  val h2t = tap.h2t(io.h2tPushStream)(3)
  val t2h = tap.t2h(io.t2hPopStream)(4)

}

case class SSTAPGenerator(address: Int,addressWidth : Int)(implicit val interconnect: BmbInterconnectGenerator) extends Area {
  val fifoDepth =4
  val jtagArea = Handle(new Area {
    val jtag = Handle(slave(Jtag()))
    val reset_sig = ClockDomain.current.readResetWire

    val sysCd = ClockDomain.current
    val jtagCd = ClockDomain(jtag.tck, reset_sig)
    val h2tPopStream = Stream(UInt(32 bits))
    val t2hPushStream = Stream(UInt(32 bits))
    val h2tFifo = StreamFifoCC(
      dataType = UInt(32 bits),
      depth = fifoDepth,
      pushClock = jtagCd,
      popClock = sysCd
    )
    val t2hFifo = StreamFifoCC(
      dataType = UInt(32 bits),
      depth = fifoDepth,
      pushClock = sysCd,
      popClock = jtagCd
    )
    val jtagArea = new ClockingArea(jtagCd) {
      val tap2 = new SSTAP()
      tap2.io.jtag <> jtag
      tap2.io.h2tPushStream >> h2tFifo.io.push
      tap2.io.t2hPopStream << t2hFifo.io.pop
    }
  })
/*
  val registersArea= Handle(new Area{
    val h2tFifo = StreamFifoCC(
      dataType = UInt(32 bits),
      depth = fifoDepth,
      pushClock = jtagArea.jtagCd,
      popClock = jtagArea.sysCd
    )
    val t2hFifo = StreamFifoCC(
      dataType = UInt(32 bits),
      depth = fifoDepth,
      pushClock = jtagArea.sysCd,
      popClock = jtagArea.jtagCd
    )
  })
*/

  val apbBridge = BmbToApb3Generator(SizeMapping(address, BigInt(1) << addressWidth))
  apbBridge.apb3Config.load(Apb3Config(
    addressWidth = addressWidth,
    dataWidth = 32
  ))
  val apbArea =  Handle(new Area {
    val apbInterface = SsTapApbInterface(Apb3Config(addressWidth = addressWidth, dataWidth = 32),
      fifoDepth)
    apbInterface.io.apbInterface<>apbBridge.output
    apbInterface.io.pushStream>>jtagArea.t2hFifo.io.push
    apbInterface.io.popStream<<jtagArea.h2tFifo.io.pop
    apbInterface.io.popOccupancy<>jtagArea.h2tFifo.io.popOccupancy
    apbInterface.io.pushOccupancy<>jtagArea.t2hFifo.io.pushOccupancy
  })

}

case class SsTapApbInterface(apb3Config:Apb3Config,fifoDepth:Int)extends Component {

  val io = new Bundle{
    val apbInterface = slave(Apb3(apb3Config))
    val popStream  = slave Stream (UInt(32 bits))
    val pushStream = master Stream (UInt(32 bits))
    val popOccupancy = in UInt(log2Up(fifoDepth+1) bits)
    val pushOccupancy = in UInt(log2Up(fifoDepth+1) bits)
  }
  val busCtrl = Apb3SlaveFactory(io.apbInterface)
  val pushValid = Reg(Bool).init(False)
  val status = Reg(Bits(16 bit))

  io.pushStream.valid := False
  io.popStream.ready := False
  when(pushValid) {
    io.pushStream.valid := True
    pushValid := False
  }
  status := ((io.pushOccupancy.asBits.resize(4 bits))) ## (io.popOccupancy.asBits.resize(4 bits) ## B"00000000") //.resize(16 bits)
  busCtrl.read(status, address = 0x10)
  busCtrl.read(io.popStream.payload, address = 0x18)
  busCtrl.drive(io.pushStream.payload, 0x1c)

  busCtrl.onWrite(0x1c)(
    {
      pushValid := True
    }
  )
  busCtrl.onRead(address = 0x18)({
    when(io.popOccupancy > 0) {
      io.popStream.ready := True
    }
  })
}