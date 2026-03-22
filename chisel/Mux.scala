package hammer

import chisel3._
import chisel3.util._

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

object MuxSel {

  /**
    * Selector, resolves to more efficient mux when conditions don't overlap with each others
    *
    * @param mapping The cases and corresponding values
    * @param otherwise The default value when none of the cases match
    * @return The final value
    */
  def apply[T <: Data](mapping: (Bool, T)*)(otherwise: T): T =
    Mux(mapping.map(_._1).reduce(_ || _), Mux1H(mapping), otherwise)
}
