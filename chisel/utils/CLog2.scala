package hammer

object CLog2 {

  /**
   * Compile Time CLog2 Utility  
   * 
   * Mostly used for calculating data width with the given range
   * 
   * Example:
   * ```scala
   * val range = 256
   * val data = UInt(CLog2(range).W)     // Create a UInt(8.W) to satisfy range = 256
   * ```  
   * 
   * @param width The width to calculate, usually this represents a maximum value
   * @return the corresponding data width
   */
  def apply(width: Int): Int = math.ceil(math.log(width) / math.log(2)).toInt
}
