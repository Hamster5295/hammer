package hammer.model

import java.lang
import scala.util.Random

class Fp(expWidth: Int, manWidth: Int)(sign: Int, exp: Int, man: Int) {

  def toFloat: Float =
    lang.Float.intBitsToFloat(
      ((sign & 1) << 31)
        | (((exp + 127) & 0xff) << 23)
        | ((man & (math.pow(2, manWidth).toInt - 1)) << (23 - manWidth))
    )

  def toBigInt: BigInt = {
    val expPow = math.pow(2, expWidth).toInt
    val manPow = math.pow(2, manWidth).toInt
    BigInt(
      (((sign & 1) << (expWidth + manWidth))
        | (((exp + expPow / 2 - 1) & (expPow - 1)) << manWidth)
        | (man & (manPow - 1)))
        .toHexString,
      16
    )
  }

  def +(other:  Fp): Float  = toFloat + other.toFloat
  def *(other:  Fp): Float  = toFloat * other.toFloat
  def *%(other: Fp): BigInt = Fp.asBigInt(this * other)
}

object Fp {
  private val rand = new Random

  def asBigInt(value: Float) =
    BigInt(lang.Float.floatToIntBits(value).toHexString, 16)

  def asBigInt(value: Double) =
    BigInt(lang.Double.doubleToLongBits(value).toHexString, 16)

  /**
    * Generate a Float-Point value with specified format
    * 
    * If you're looking for standard formats, see `f16`, `b16`, `f8e3`, `f8e4` and `f8e5`
    *
    * @param expWidth The width of the exponent
    * @param manWidth The width of the mantissa
    * @return The random Float-Point value with width = (expWidth + manWidth + 1)
    */
  def random(expWidth: Int, manWidth: Int): Fp = {
    val bias = (math.pow(2, expWidth).toInt - 2) / 2

    new Fp(expWidth, manWidth)(
      if (rand.nextBoolean()) 1 else 0,
      rand.nextInt(bias * 2) - bias + 1,
      rand.nextInt(math.pow(2, manWidth).toInt)
    )
  }

  /**
    * Generate a Float16
    *
    * @return The random value
    */
  def f16: Fp = random(5, 10)

  /**
    * Generate a BFloat16
    *
    * @return The random value
    */
  def b16: Fp = random(8, 7)

  /**
    * Generate a Float8 E3M4
    *
    * @return The random value
    */
  def f8e3: Fp = random(3, 4)

  /**
    * Generate a Float8 E4M3
    *
    * @return The random value
    */
  def f8e4: Fp = random(4, 3)

  /**
    * Generate a Float8 E5M2
    *
    * @return The random value
    */
  def f8e5: Fp = random(5, 2)
}
