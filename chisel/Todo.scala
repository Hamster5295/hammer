package hammer

import chisel3._
import scala.io.AnsiColor._

object TODO {

  /**
    * Create a placeholder with 0.U as its value
    * 
    * When you export your project to verilog or doing unit tests, `TODO` will emit a warning with your custom message in case you miss it
    * 
    * @param message The custom message. You can provide more information here
    * @return `0.U`
    */
  def apply(message: String, chiselType: Data = 0.U): Data = {
    val trace = Thread.currentThread().getStackTrace()(2)
    println(
      s"${YELLOW}TODO unimplemented at ${trace.getFileName()}:${trace.getLineNumber()}: $message$RESET"
    )
    Zero(chiselType)
  }
}
