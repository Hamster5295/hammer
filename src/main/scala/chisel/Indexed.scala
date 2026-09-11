package hammer

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class Indexed[+T <: Data](gen: T, indexWidth: Int) extends Bundle {
  val index = UInt(indexWidth.W)
  val bits  = gen
}

object Indexed {

  /**
    * Create a indexed signal from a raw one
    *
    * @param gen The raw signal to be wrapped
    * @param index The index of the signal
    * @param indexWidth The width of the index
    * @return
    */
  def apply[T <: Data](gen: T, index: Int)(indexWidth: Int) = {
    val value = Wire(new Indexed(chiselTypeOf(gen), indexWidth))
    value.index := index.U
    value.bits  := gen
    value
  }
}
