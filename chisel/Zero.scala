package hammer

import chisel3._

object Zero {

  /**
    * Tie any chisel typed Data down to **zero**
    * 
    * This is exactly what https://www.chisel-lang.org/docs/cookbooks/cookbook#how-can-i-tieoff-a-bundlevec-to-0 does
    *
    * @param data The data type to be tied down. We don't care about its value here.
    * @return The zero corresponding to the given type
    */
  def apply[T <: Data](data: T): T = 0.U.asTypeOf(data)
}

object RegZero {

  /**
    * Create a register with 0 as reset value
    *
    * @param t The data type of the reg
    * @return The register
    */
  def apply[T <: Data](t: T): T = RegInit(Zero(t))
}


object WireZero {

  /**
    * Create a wire with 0 as reset value
    *
    * @param t The data type of the wire
    * @return The wire
    */
  def apply[T <: Data](t: T): T = WireInit(Zero(t))
}
