package hammer

import chisel3._
import chisel3.util._

object InvPriorityEncoder {

  /**
    * Priority Encoder, but selects the MSB of the input sequence
    * 
    * @example {{{
    * val result = InvPriorityEncoder("b000101".U)  // result = 3
    * }}}
    * 
    * @param in
    * @return
    */
  def apply(in: UInt) = PriorityMux(
    Reverse(in).asBools,
    (0 until in.getWidth).map(_.asUInt).reverse,
  )
}
