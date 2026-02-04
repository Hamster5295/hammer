package hammer

import chisel3.UInt
import chisel3.util.Fill

object SignExt {

  /**
    * Sign-Extend the value to specified length
    *
    * @param input The value to sign-extend
    * @param len The target length
    * @return The extended value
    */
  def apply(input: UInt, len: Int): UInt =
    if (input.getWidth >= len) return input
    else return Fill(len - input.getWidth, input.head(1)) ## input
}
