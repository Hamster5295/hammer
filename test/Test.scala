package hammar.test

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.FailedExpectationException
import chisel3.simulator.PeekPokeAPI
import io.AnsiColor._

object Test extends PeekPokeAPI {

  /**
    * Run specified test on a specified dut
    * 
    * This util allow you to write unit tests like this:
    * ```scala
    * "Module should function correctly" in {
    *   simulate(new DeviceUnderTest()) { dut => 
    *       Test("A", dut) { dut => /* Do sth */ }
    *       Test("B", dut) { dut => /* Do sth */ }
    *   }
    * }
    * ```
    * 
    * instead of the traditional method:
    * ```scala
    * "Module should do A correctly" in {
    *   simulate(new DeviceUnderTest()) { dut => 
    *       // Do sth
    *   }
    * }
    * 
    * "Module should do B correctly" in {
    *   simulate(new DeviceUnderTest()) { dut => 
    *       // Do sth
    *   }
    * }
    * ```
    * which would call `verilator` for each test even if `dut` is completely the same, which is really slow and inefficient
    *
    * @param name The name of the test
    * @param dut
    * @param stimulus
    */
  def apply[T <: Module](name: String, dut: T)(stimulus: T => Unit): Unit = {

    print(s"[${YELLOW}Running${RESET}] $name\n")

    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
    dut.clock.step()

    var err: Option[FailedExpectationException[?]] = None

    try {
      stimulus(dut)
    } catch {
      case e: FailedExpectationException[?] => err = Some(e)
    }

    if (err.isEmpty) println(f"\u001b[A[${GREEN}Passed${RESET}] $name")
    else {
      println(f"\u001b[A[${RED}Failed${RESET}] $name")
      throw err.get
    }
  }
}

object Expect extends PeekPokeAPI {
  def apply[T](
      data:     UInt,
      expected: BigInt
  )(preprocess: BigInt => BigInt)(implicit sourceInfo: SourceInfo): Unit = {
    val observed = preprocess(data.peekValue().asBigInt)
    if (observed != expected)
      throw FailedExpectationException(
        observed,
        expected,
        s"Expectation failed: observed value ${observed} != ${expected}",
        sourceInfo
      )
  }
}
