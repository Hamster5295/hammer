package hammer

import chisel3._

class Float(expWidth: Int, manWidth: Int) extends Bundle {
  val sign = Bool()
  val exp  = UInt(expWidth.W)
  val man  = UInt(manWidth.W)
}

object Float {
  def apply(expWidth: Int, manWidth: Int)(bits: UInt): Float = {

    require(
      bits.widthKnown,
      "The width of 'bits' should be known. If you're sure the width is known, check if RegNext presents."
    )
    require(
      bits.getWidth == 1 + expWidth + manWidth,
      s"The width of 'bits' [${bits.getWidth}] != Provided schema [${1 + expWidth + manWidth}]"
    )
    require(
      expWidth > 1,
      s"ExpWidth should be larget than 1"
    )
    require(
      manWidth > 0,
      s"ExpWidth should be larget than 0"
    )

    Float(bits.msb(), bits.get(-2, -1 - expWidth), bits.end(manWidth))
  }

  def apply(sign: Bool, exp: UInt, man: UInt): Float = {
    val data = Wire(new Float(exp.getWidth, man.getWidth))
    data.sign := sign
    data.exp  := exp
    data.man  := man
    data
  }
}
