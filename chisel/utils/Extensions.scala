import chisel3._
import chisel3.util.Fill

package object hammer {

  implicit class UIntExtensions(self: UInt) {

    /**
      * Get the lower X bits (i.e. lsb) from the data provided
      *
      * @param len The length
      * @return
      */
    def end(len: Int): UInt = self(len - 1, 0)

    /**
      * Get the nth msb of the data
      * 
      * Example:
      * ```scala
      * "b10101".U.msb(1)  // Gets the second highest bit 0
      * ```
      *
      * @param idx
      * @return
      */
    def msb(idx: Int = 0): Bool = self(self.getWidth - 1 - idx)

    /**
      * Get a block of data from UInt.
      * The block is defined by its size.
      * 
      * This is equal to
      * ```scala
      * value(index * (size + 1) - 1, index * size)
      * ```
      * 
      * @param index
      * @param size
      * @return
      */
    def block(index: Int, size: Int): UInt =
      self(index * size + size - 1, index * size)

    /**
      * Pad at the lsb of the UInt to specified width
      * 
      * Example
      * ```scala
      * "b1010".U.rpad(6)       // b101000
      * "b1010".U.rpad(6, 1)    // b101011
      * ```
      *
      * @param width The target width
      * @param bit The filling bit, 0 for 0 and non-0 for 1
      * @return
      */
    def rpad(width: Int, bit: Int = 0): UInt =
      if (self.getWidth >= width) self
      else self ## Fill(width - self.getWidth, (if (bit == 0) 0 else 1).U)

  }

  implicit class VecExtensions[T <: Data](self: Vec[T]) {

    /**
      * Get a subset of Vec
      * 
      * Example:
      * ```scala
      * val data = Vec(true.B, true.B, false.B, true.B) // 4 elements
      * data.get(1, 3)      // Gets the 2nd & 3rd elements of the original Vec
      * ```
      * 
      * @param start The starting index, inclusive
      * @param end The ending index, exclusive
      * @return
      */
    def get(start: Int, end: Int): Vec[T] = {
      require(self.length > 0, s"input length = ${self.length} should > 0")
      require(
        self.length >= end,
        s"input length = ${self.length} should bigger than end = $end"
      )

      val result = Wire(Vec(end - start, chiselTypeOf(self(0))))
      val len    = self(0).getWidth
      val bits   = self.asUInt

      for (x <- 0 until (end - start)) {
        result(x) := bits((x + start) * len + len - 1, (x + start) * len)
      }

      result
    }

    /**
      * Get a block of data from the original Vec
      * 
      * This is equal to 
      * ```scala
      * data.get(index * size, (index + 1) * size)
      * ````
      * 
      * @param index The block index
      * @param size The size of each block
      * @return
      */
    def block(index: Int, size: Int): Vec[T] =
      get(index * size, index * size + size)

  }

}
