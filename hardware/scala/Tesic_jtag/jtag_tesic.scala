package mylib

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
//class FifoT2h [T <: Data](dataType: HardType[T], depth: Int) extends  Component {
//    val io = new Bundle {
//    val push = in UInt(widthOf(dataType))
//    val pop = out UInt(widthOf(dataType))
//    val flush = in Bool() default(False)
//    val read= out UInt(widthOf(dataType))
//    val occupancy    = out UInt (log2Up(depth + 1) bits)
//    val availability = out UInt (log2Up(depth + 1) bits)
//  }
//  val logic = (depth > 1) generate new Area {
//    val ram = Mem(dataType, depth)
//    val pushPtr = Counter(depth)
//    val popPtr = Counter(depth)
//    val ptrMatch = pushPtr === popPtr
//    val risingOccupancy = RegInit(False)
//    val pushing = io.push.fire
//    val popping = io.pop.fire
//    val empty = ptrMatch & !risingOccupancy
//    val full = ptrMatch & risingOccupancy
//    val reading =io.read.fire
//    io.push.ready := !full
//    io.pop.valid := !empty & !(RegNext(popPtr.valueNext === pushPtr, False) & !full) //mem write to read propagation
//    io.pop.payload := ram.readSync(popPtr.valueNext)
//
//    when(pushing =/= popping) {
//      risingOccupancy := pushing
//    }
//    when(pushing) {
//      ram(pushPtr.value) := io.push.payload
//      pushPtr.increment()
//    }
//    when(popping) {
//      popPtr.increment()
//    }
//    when(popping) {
//      popPtr
//    }
//
//    val ptrDif = pushPtr - popPtr
//    if (isPow2(depth)) {
//      io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).asUInt
//      io.availability := ((!risingOccupancy && ptrMatch) ## (popPtr - pushPtr)).asUInt
//    } else {
//      when(ptrMatch) {
//        io.occupancy    := Mux(risingOccupancy, U(depth), U(0))
//        io.availability := Mux(risingOccupancy, U(0), U(depth))
//      } otherwise {
//        io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
//        io.availability := Mux(pushPtr > popPtr, U(depth) + (popPtr - pushPtr), (popPtr - pushPtr))
//      }
//    }
//
//    when(io.flush){
//      pushPtr.clear()
//      popPtr.clear()
//      risingOccupancy := False
//    }
//  }
//}
//}
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

  val shifter = Reg(Bits(widthOf(data.payload)+1 bit))
  val validDrShift =RegNext(ctrl.capture&&ctrl.enable)
  // we don't want to pop anything from the fifo unless we need it
  data.ready:=False
  when(ctrl.enable) {
    when(ctrl.capture) {
      // the v bit is connected to fifo.pop.valid
      shifter := B(data.payload)##data.valid
      data.ready:=True
    }

    when(ctrl.shift) {
      shifter := (ctrl.tdi ## shifter) >> 1
    }
  }
  ctrl.tdo := shifter.lsb
}

class JtagTap_tesic(jtag: Jtag, instructionWidth: Int) extends JtagTap(jtag, instructionWidth){

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