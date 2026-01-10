package hammer

import chisel3.Bool
import chisel3.Data
import chisel3.util.MuxCase

object MuxIf {
  /**
    * Similar to `MuxCase`, but the default value is put to the end of arguments 
    *
    * @param mapping The cases and corresponding values
    * @param otherwise The default value when none of the cases match
    * @return The final value
    */
  def apply[T <: Data](mapping: (Bool, T)*)(otherwise: T): T =
    MuxCase(otherwise, mapping)
}
