package hammer

object Pow2 {
  /**
    * Similar to math.pow(2, i), but is designed for Int calculations
    *
    * @param i The number to be powered, which is likely a data width
    * @return 2 ^ i
    */
  def apply(i: Int): Int = math.pow(2, i).toInt
}
