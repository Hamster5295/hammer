package hammer

import chisel3._
import chisel3.experimental.requireIsHardware
import chisel3.reflect.DataMirror

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

object RegOut {

  /**
    * Wraps an IO with output signals applying a Register
    *
    * Example:
    * ```scala
    * val module = Module(new Adder)
    * val adderIO = RegOut(module.io)   // Now `adderIO` can be used for input/output
    * 
    * // `data` will be delayed for 1 cycle as register is inserted
    * val data = adderIO.output         
    * ```
    * 
    * @param io The io to apply 
    * @return The buffered bundle
    */
  def apply[T <: Bundle](io: T): T = {
    val out = Wire(chiselTypeOf(io))
    for ((name, data) <- out.elements) {
      if (DataMirror.specifiedDirectionOf(data) == SpecifiedDirection.Output) {
        data := RegNext(io.elements(name))
      } else {
        io.elements(name) := data
      }
    }
    out
  }
}

