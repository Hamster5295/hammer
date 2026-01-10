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
  def sint(width: Int): BigInt = BigInt(width, rand) - BigInt("1" + "0" * (width - 1), 2)
  
  /**
    * Generate a random unsigned integer with specified width
    *
    * @param width The width of the integer
    * @return The random unsigned integer
    */
  def uint(width: Int): BigInt = BigInt(width, rand)
}
