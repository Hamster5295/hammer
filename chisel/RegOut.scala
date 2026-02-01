package hammer

import chisel3._
import chisel3.reflect.DataMirror

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
