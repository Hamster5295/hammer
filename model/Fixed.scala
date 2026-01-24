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
    * Example
    * ```scala
    * val mask = Fixed.mask(5)  // b11111
    * ```
    *
    * @param width The width of the bitmask
    * @return The bitmask (i.e. 111111...1)
    */
  def mask(width: Int): BigInt = BigInt("1" * width, 2)
}
