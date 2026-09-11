import chisel3._
import chisel3.util._
import java.lang.Math._

package object hammer {

  implicit class UIntExt(self: UInt) {

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
      * @example {{{
      * "b10101".U.msb(1)  // Gets the second highest bit 0
      * }}}
      *
      * @param idx
      * @return
      */
    def msb(idx: Int = 0): Bool = self(self.getWidth - 1 - idx)

    /**
      * Similar to UInt.extract, with negative index support
      * 
      * @example {{{
      * "b10001".get(-1)    // 1
      * "b10001".get(-2)    // 0
      * }}}
      * 
      * @param idx The index to get
      * @return The target bit
      */
    def get(idx: Int): Bool =
      self(if (idx < 0) self.getWidth + idx else idx)

    /**
      * Similar to UInt.extract, with negative index support
      * 
      * @example {{{
      * "b10001".get(-1, -2)    // b10
      * "b10001".get(-2, -5)    // b0001
      * }}}
      * 
      * @param left The left border to get (inclusive)
      * @param right The right border to get (inclusive)
      * @return The target bit
      */
    def get(left: Int, right: Int): UInt =
      self(
        if (left < 0) self.getWidth + left else left,
        if (right < 0) self.getWidth + right else right,
      )

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

    /*
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
    def block(index: UInt, size: Int): UInt =
      VecInit(Seq.tabulate(self.getWidth / size)(i =>
        self((i + 1) * size - 1, i * size),
      ))(index)

    /**
      * Get a span of data from UInt by a lsb position and size
      * 
      * This is equal to
      * ```scala
      * value(lsb + size - 1, lsb)
      * ```
      * 
      * @example {{{
      * "011010".U.span(2, 3)   // b110
      * }}}
      *
      * @param lsb The lsb of the span
      * @param size The size of the span
      * @return
      */
    def span(lsb: Int, size: Int): UInt = self(lsb + size - 1, lsb)

    /**
      * Get a span of data from UInt by a msb position and size
      * 
      * This is equal to
      * ```scala
      * value(msb, msb - size + 1)
      * ```
      * 
      * @example {{{
      * "011010".U.span(4, 3)   // b110
      * }}}
      *
      * @param msb The msb of the span
      * @param size The size of the span
      * @return
      */
    def rspan(msb: Int, size: Int): UInt = self(msb, msb - size + 1)

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

    /**
      * Convert UInt to binary string
      *
      * @return The binary string with minimum required width
      */
    def toBin = self.litValue.toString(2)

    /**
      * Convert UInt to binary string with specified width
      * 
      * If len < minimum required width, then the result is truncated  
      * If len > minimum required width, '0's are appended on the msb
      *
      * @param len The target length
      * @return The binary string
      */
    def toBin(len: Int): String = {
      val origin = self.litValue.toString(2)
      if (origin.length() >= len) return origin.substring(0, len)
      else return "0" * (len - origin.length()) + origin
    }

    /**
      * Check whether UInt matches any of the patterns provided
      * 
      * Equivalent to (data === pat1) || (data === pat2) || ...
      *
      * Example
      * ```scala
      * val isLowerThan4 = data.in(0.U, 1.U, 2.U, 3.U)
      * ```
      * 
      * @param patterns The patterns to match
      * @return
      */
    def in(patterns: UInt*): Bool = patterns.map(i => self === i).reduce(_ || _)

    /**
      * Align current UInt, tying end X bits down to zero
      *
      * @param endBits The end X bits to be tied
      * @return The aligned UInt
      */
    def align(endBits: Int): UInt = self.get(-1, endBits) ## 0.U(endBits.W)

    /**
      * Fix an UInt into specific width
      *
      * @param width The width specified
      * @return A wired UInt with fixed width
      */
    def width(width: Int): UInt = {
      val wire = WireZero(UInt(width.W))
      if (self.isWidthKnown) {
        if (self.getWidth < width) wire := self
        else wire                       := self.end(width)
      } else {
        wire := self
      }

      wire
    }

    /**
      * Split the UInt into multiple shorter slices
      * 
      * Will return 1 slice when target width is larger than the UInt itself
      *
      * @param width The width to be splitted to
      * @return The splited wired Vec
      */
    def split(width: Int): Vec[UInt] = {
      require(width > 0, "The UInt can't be splited to 0-widthed slices")
      require(
        self.widthKnown,
        "The UInt should have fixed width to be splitted",
      )

      val cnt = math.ceil(self.getWidth.toFloat / width).toInt
      val vec = WireZero(Vec(cnt, UInt(width.W)))

      Seq.tabulate(cnt)(n =>
        vec(n) := self(math.min((n + 1) * width - 1, self.getWidth), n * width),
      )
      vec
    }
  }

  implicit class VecExt[T <: Data](self: Vec[T]) {

    /**
      * Get a subset of Vec
      * 
      * @example {{{
      * val data = Vec(true.B, true.B, false.B, true.B) // 4 elements
      * data.get(1, 3)      // Gets the 2nd & 3rd elements of the original Vec
      * }}}
      * 
      * @param start The starting index, inclusive
      * @param end The ending index, exclusive
      * @return
      */
    def get(start: Int, end: Int): Vec[T] = {
      require(self.length > 0, s"input length = ${self.length} should > 0")
      require(
        self.length >= end,
        s"input length = ${self.length} should bigger than end = $end",
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

    /**
      * Create a Seq of `DataWithIndex` with original Vec
      * 
      * The `idx` of each element is an UInt Wire of the corresponding index, which is really helpful in constructing a hardware list or map.
      *
      * Note that the returned `Indexed[T]` is a new Hardware Type and should not be connected as a sink
      * 
      * @param idxWidth The width of the `idx` wire
      * @return 
      */
    def withIndex(idxWidth: Int): Seq[Indexed[T]] =
      self.zipWithIndex.map { case (data, idx) =>
        Indexed(data, idx)(idxWidth)
      }

    def elemOp[B, R <: Data](other: Seq[B])(op: (T, B) => R): Vec[R] =
      if (self.length != other.length) throw new RuntimeException(
        s"elemOp sources $self (length = ${self.length}) and $other (length = ${other.length}) has different length!",
      )
      else {
        val vec = Wire(Vec(self.length, chiselTypeOf(op(self(0), other(0)))))
        vec.zipWithIndex.map { case (el, idx) =>
          el := self.zip(other).map { case (s, o) => op(s, o) }(idx)
        }
        return vec
      }

  }

  implicit class SeqExt[T <: Data](self: Seq[T]) {

    /**
      * Reduce a seq of Data by tree  
      * 
      * This is really similar to Chisel's Vec.reduceTree, with slight difference on ops
      * 
      * i.e. The ops now receives another Int parameter indicating the layer index
      *
      * @param reduceOp The reduce operation
      * @param layerOp The operation when this element is bypassed to the next layer (e.g. not selected due to not being a power of 2)
      * @return The reduced value
      */
    def treeReduce(
        reduceOp: (Int, T, T) => T,
        layerOp:  (Int, T) => T = (_: Int, x: T) => x,
    ) = {
      require(self.length > 0, "Cannot apply reduction on a seq of size 0")

      def recReduce[T](
          seq:      Seq[T],
          reduceOp: (Int, T, T) => T,
          layerOp:  (Int, T) => T,
          layer:    Int,
      ): T = {
        val n = seq.length
        n match {
          case 1 => layerOp(layer, seq(0))
          case 2 => reduceOp(layer, seq(0), seq(1))
          case _ =>
            val m = pow(
              2,
              floor(log10(n - 1) / log10(2)),
            ).toInt // number of nodes in next level, will be a power of 2
            val p = 2 * m - n // number of nodes promoted

            val l = seq.take(p).map(i => layerOp(layer, i))
            val r = seq
              .drop(p)
              .grouped(2)
              .map { case Seq(a, b) =>
                reduceOp(layer, a, b)
              }
              .toVector
            recReduce(l ++ r, reduceOp, layerOp, layer + 1)
        }
      }

      recReduce(self, reduceOp, layerOp, 0)
    }

    /**
      * Reduce a seq of Data by tree  
      * 
      * This is really similar to Chisel's Vec.reduceTree, with slight difference on ops
      * 
      * i.e. The ops now receives 2 Int parameter indicating the layer index and reduction op index respectively
      *
      * @param reduceOp The reduce operation
      * @param layerOp The operation when this element is bypassed to the next layer (e.g. not selected due to not being a power of 2)
      * @return The reduced value
      */
    def treeReduce(
        reduceOp: (Int, Int, T, T) => T,
        layerOp:  (Int, T) => T,
    ) = {
      require(self.length > 0, "Cannot apply reduction on a seq of size 0")

      var idx = 0

      def recReduce[T](
          seq:      Seq[T],
          reduceOp: (Int, Int, T, T) => T,
          layerOp:  (Int, T) => T,
          layer:    Int,
      ): T = {
        val n = seq.length
        n match {
          case 1 => layerOp(layer, seq(0))
          case 2 => {
            idx += 1
            return reduceOp(layer, idx - 1, seq(0), seq(1))
          }
          case _ =>
            val m = pow(
              2,
              floor(log10(n - 1) / log10(2)),
            ).toInt // number of nodes in next level, will be a power of 2
            val p = 2 * m - n // number of nodes promoted

            val l = seq.take(p).map(i => layerOp(layer, i))
            val r = seq
              .drop(p)
              .grouped(2)
              .map { case Seq(a, b) =>
                idx += 1
                reduceOp(layer, idx - 1, a, b)
              }
              .toVector
            recReduce(l ++ r, reduceOp, layerOp, layer + 1)
        }
      }

      recReduce(self, reduceOp, layerOp, 0)
    }

    /**
      * Transform the Seq into UInt by concating all the bits
      *
      * @return The concated UInt
      */
    def asUInt: UInt = self.asVec.asUInt

    /**
      * Transform the Seq into Vec
      * 
      * This is equal to
      * ```scala
      * VecInit(seq)
      * ```
      *
      * @return The vector
      */
    def asVec: Vec[T] = VecInit(self)
  }
}
