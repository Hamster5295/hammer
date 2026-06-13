package hammer

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class Indexed[+T <: Data](gen: T, indexWidth: Int) extends Bundle {
  val index = UInt(indexWidth.W)
  val bits  = gen
}

object Indexed {
  def apply[T <: Data](gen: T, index: Int)(indexWidth: Int) = {
    val value = Wire(new Indexed(chiselTypeOf(gen), indexWidth))
    value.index := index.U
    value.bits  := gen
    value
  }
}
