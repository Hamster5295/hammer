package hammer.test

import chisel3.experimental.SourceInfo
import chisel3.simulator.FailedExpectationException

object Fail {
  def apply(msg: String) = throw new RuntimeException(msg)

  def apply(observed: BigInt, expected: BigInt, msg: String)(implicit
      sourceInfo: SourceInfo,
  ) =
    throw FailedExpectationException(
      observed,
      expected,
      s"${msg} \nobserved = 0x${observed.toString(16)} \nexpected = 0x${expected.toString(16)}\n",
      sourceInfo,
    )
}
