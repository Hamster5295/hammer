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
  def apply(data: Data) = 0.U.asTypeOf(chiselTypeOf(data))
}
