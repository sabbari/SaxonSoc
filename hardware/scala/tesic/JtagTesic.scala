package tesic

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.com.jtag.JtagState.{DR_CAPTURE, DR_EXIT1, DR_EXIT2, DR_SHIFT}
import spinal.lib.com.jtag.{Jtag, JtagFsm, JtagTap, JtagTapInstructionCtrl, JtagTapInstructionRead, JtagTapInstructionWrite}

import scala.util.Random


class JtagTapInstructionBypass[T <: Data] extends Area {
  val ctrl = JtagTapInstructionCtrl()
  ctrl.tdo:=False
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
  val counter=Reg(UInt(log2Up( widthOf(data.payload)) +1 bit))
  when(ctrl.enable) {
    when(ctrl.capture){
      // The v bit is connected to fifo.push.ready byte, no Dr is accepted if we can't write in the stream
      shifter:=(shifter##data.ready.asBits(1 bit)).resize(widthOf(data.payload)bit)
    }
    when(ctrl.shift) {
      shifter := (ctrl.tdi ## shifter) >> 1
      counter:= counter +1
    }otherwise {
      //Counter should  always be equal to zero unless we are in a DR SHIFT
      counter:=0
    }
  }
  ctrl.tdo := shifter.lsb
  data << inStream
  inStream.payload.assignFromBits(shifter)
  //our data is valid only when we receive all of the bits, happens when counter ===....
  inStream.valid:=(counter===widthOf(data.payload))
}
class JtagTapInstructionT2h(data: Stream[UInt]) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val shifter= Reg(Bits(widthOf(data.payload)+1 bit))
  //used to store poped data during a peek
  val buffer = Reg(Bits(widthOf(data.payload)+1 bit))
  val validDrShift =RegNext(ctrl.capture&&ctrl.enable)
  val counter=Reg(UInt(1 bit))
  // we don't want to pop anything from the fifo unless we need it
  val lastStateIsPeek,thisStateIsPeek=Reg(Bool).init(False)
  data.ready:=False
  when(ctrl.enable) {
    when(ctrl.capture) {
      //forcing a pop when the last peek had an lsb =0
      when(lastStateIsPeek & buffer.lsb ) {
        shifter := buffer
      } otherwise {
        data.ready := True
        buffer := B(data.payload) ## data.valid
        shifter := B(data.payload) ## data.valid //buffer//B(data.payload)##data.valid
      }
    }
    when(ctrl.shift) {
      counter := counter + 1
      shifter := (ctrl.tdi ## shifter) >> 1
      when(counter === 0) {
        thisStateIsPeek :=  ctrl.tdi
        lastStateIsPeek := thisStateIsPeek
      }
    }otherwise {
      counter := 0
    }
  }

  ctrl.tdo := shifter.lsb
}

class JtagTapTesic(jtag: Jtag, instructionWidth: Int) extends JtagTap(jtag, instructionWidth){

  def bypass( light : Boolean = false)(instructionId: Int) = {
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