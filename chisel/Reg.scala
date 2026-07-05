package hammer

import chisel3._
import chisel3.experimental.requireIsHardware
import chisel3.reflect.DataMirror
import chisel3.util.RegEnable

object RegNxt {

  /**
    * Similar to RegNext, but the output width is specified
    *
    * @param next The next value
    * @return The registered signal
    */
  def apply[T <: Data](next: T) = {
    val reg = Reg(chiselTypeOf(next))
    requireIsHardware(next, "reg next")
    reg := next
    reg
  }

  /**
    * Similar to RegNext, but the output width is specified
    *
    * @param next The next value
    * @param init The reset value
    * @return The registered signal
    */
  def apply[T <: Data](next: T, init: T) = {
    val reg = RegInit(chiselTypeOf(next), init)
    requireIsHardware(next, "reg next")
    reg := next
    reg
  }
}

object RegFlush {
  /**
    * A flushable register
    * 
    * Priority: flush > enable
    *
    * @param next The next value to be written
    * @param enable The enable signal that allows data writes
    * @param flush The flush signal that clears the register
    * @return
    */
  def apply[T <: Data](
      next:   T,
      enable: Bool,
      flush:  Bool,
  ): T = {
    val r = RegInit(Zero(next))
    r := MuxIf(
      flush   -> Zero(next),
      !enable -> r,
    )(next)
    r
  }
}
