package hammer

import chisel3._
import chisel3.util._
import hammer.model.Fixed

class SaturateCounterIO(width: Int) extends Bundle {
  val enable = Input(Bool())
  val op     = Input(Bool())
  val value  = Output(UInt(width.W))
  val next   = Output(UInt(width.W))
}

class SaturateCounter(width: Int, init: BigInt) extends Module {
  val io    = IO(new SaturateCounterIO(width))
  val value = RegInit(init.U(width.W))

  val isMax = value === Fixed.mask(width).U(width.W)
  val isMin = value === 0.U(width.W)

  val next = MuxIf(
    !io.enable                          -> value,
    (isMax && io.op || isMin && !io.op) -> value,
    io.op                               -> (value +% 1.U),
    !io.op                              -> (value -% 1.U)
  )(value)

  value := next

  io.value := value
  io.next  := next
}

object SaturateCounter {
  def apply(width: Int, init: BigInt) = new SaturateCounter(width, init)
}
