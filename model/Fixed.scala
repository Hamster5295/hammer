package hammer.model

import scala.util.Random

object Fixed {
  private val rand = new Random

  /**
    * Generate a random signed integer with specified width
    *
    * @param width The width of the integer, including the sign
    * @return The random signed integer
    */
  def sint(width: Int): BigInt =
    BigInt(width, rand) - BigInt("1" + "0" * (width - 1), 2)

  /**
    * Generate a random unsigned integer with specified width
    *
    * @param width The width of the integer
    * @return The random unsigned integer
    */
  def uint(width: Int): BigInt = BigInt(width, rand)

  /**
    * Generate a bitmask with specified width
    * 
    * @example {{{
    * val mask = Fixed.mask(5)  // b11111
    * }}}
    *
    * @param width The width of the bitmask
    * @return The bitmask (i.e. 111111...1)
    */
  def mask(width: Int): BigInt = BigInt("1" * width, 2)

  /**
    * Transform a byte mask to bitmask
    * 
    * @example {{{
    * val byteMask = 0b1101
    * val bitMask = byte2bitMask(byteMask)  // 0xff_ff_00_ff
    * }}}
    *
    * @param byteMask
    * @return
    */
  def byte2bitMask(byteMask: BigInt): BigInt = (for (i <- 0 until byteMask.bitLength)
    yield
      (if (((byteMask >> i) & 1) == 1) 0xff else 0x00) << (8 * i))
    .reduce(_ | _)
}
