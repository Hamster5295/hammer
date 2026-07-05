package hammer

import chisel3._
import chisel3.experimental.requireIsHardware
import chisel3.util._
import hammer.model.Fixed

class SaturateCounterIO(width: Int) extends Bundle {
  val enable   = Input(Bool())
  val op       = Input(Bool())
  val set      = Input(Bool())
  val setValue = Input(UInt(width.W))

  val value = Output(UInt(width.W))
  val next  = Output(UInt(width.W))
}

/**
  * A Saturate Counter
  * 
  * **Signals**
  * - `enable`:   active high, enables the `op` operation
  * - `op`:       0 -> counter -= 1; 1 -> counter += 1
  * - `set`:      active high, enables counter value replacement
  * - `setValue`: the value to be replaced into counter
  * 
  * - `value`: the output value
  * - `next`:  the next value that counter will take
  *
  * @param width the counter's width
  * @param init the initial value
  */
class SaturateCounter(width: Int, init: BigInt) extends Module {
  val io    = IO(new SaturateCounterIO(width))
  val value = RegInit(init.U(width.W))

  val isMax = value === Fixed.mask(width).U(width.W)
  val isMin = value === 0.U(width.W)

  val next = MuxIf(
    io.set                                                -> io.setValue,
    (!io.enable || (isMax && io.op) || (isMin && !io.op)) -> value,
    io.op                                                 -> (value +% 1.U),
    !io.op                                                -> (value -% 1.U),
  )(value)

  value := next

  io.value := value
  io.next  := next
}

object SaturateCounter {
  /**
    * Create a Saturate Counter with specified data width and initial value
    *
    * @param width The counter's data width
    * @param init The initial value
    * @return
    */
  def apply(width: Int, init: BigInt): SaturateCounter =
    new SaturateCounter(width, init)

  /**
    * Create a Saturate Counter with a Hardware data
    *
    * @param data The hardware data to be connected as output
    * @param init The initial value
    * @return
    */
  def apply(data: UInt, init: BigInt = 0): SaturateCounter = {
    requireIsHardware(data, "Only Hardware can be wrapped by a counter module!")

    val cnter = Module(SaturateCounter(data.getWidth, init))
    data := cnter.io.value

    cnter
  }
}
