package hammer

import chisel3._
import chisel3.util._

class DataWithIndex[T <: Data](gen: => T, idxWidth: Int) extends Bundle {
  val data = chiselTypeOf(gen)
  val idx  = UInt(idxWidth.W)
}

object DataWithIndex {
  def apply[T <: Data](gen: => T, idx: Int, idxWidth: Int) = {
    val bundle = Wire(new DataWithIndex(gen, idxWidth))
    bundle.data := gen
    bundle.idx  := idx.U
    bundle
  }
}
