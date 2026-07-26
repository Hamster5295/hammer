package hammer.test

import chisel3._
import chisel3.util._
import chisel3.simulator._
import chisel3.experimental.SourceInfo

object Expect extends PeekPokeAPI {
  def apply[T](
      data:     UInt,
      expected: BigInt,
  )(preprocess: BigInt => BigInt)(implicit sourceInfo: SourceInfo): Unit = {
    val observed = preprocess(data.peek().litValue)
    if (observed != expected)
      Fail(observed,expected, "Expectation FAILED")
  }
}