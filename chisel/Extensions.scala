import chisel3._
import chisel3.experimental.Targetable.TargetableSyntax
import chisel3.reflect.DataMirror
import chisel3.util._
import java.lang.Math._
import os.Source.WritableSource
import scala.reflect.runtime.universe._

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
      * Similar to UInt.extract, with negative index support
      * 
      * Example:
      * ```scala
      * "b10001".get(-1)    // 1
      * "b10001".get(-2)    // 0
      * ```
      * 
      * @param idx The index to get
      * @return The target bit
      */
    def get(idx: Int): Bool =
      self(if (idx < 0) self.getWidth + idx else idx)

    /**
      * Similar to UInt.extract, with negative index support
      * 
      * Example:
      * ```scala
      * "b10001".get(-1, -2)    // b10
      * "b10001".get(-2, -5)    // b0001
      * ```
      * 
      * @param left The left border to get (inclusive)
      * @param right The right border to get (inclusive)
      * @return The target bit
      */
    def get(left: Int, right: Int): UInt =
      self(
        if (left < 0) self.getWidth + left else left,
        if (right < 0) self.getWidth + right else right
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

  implicit class VecExt[T <: Data](self: Vec[T]) {

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
        layerOp:  (Int, T) => T = (_: Int, x: T) => x
    ) = {
      require(self.length > 0, "Cannot apply reduction on a seq of size 0")

      def recReduce[T](
          seq:      Seq[T],
          reduceOp: (Int, T, T) => T,
          layerOp:  (Int, T) => T,
          layer:    Int
      ): T = {
        val n = seq.length
        n match {
          case 1 => layerOp(layer, seq(0))
          case 2 => reduceOp(layer, seq(0), seq(1))
          case _ =>
            val m = pow(
              2,
              floor(log10(n - 1) / log10(2))
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
  }

  implicit class BundleExt(self: Bundle) {

    /**
      * Passthough a bundle to another in a submodule, connecting all signals of same name
      * 
      * Really helpful if you are using a Wrapper-Inner pattern
      *
      * @param sub The IO in a submodule
      * @param debug Print all the connections when you think this operator is not functioning normally
      */
    def :>>(sub: Bundle)(implicit debug: Boolean = false): Unit = {

      // Reflect to get all signals in a bundle
      val ru    = scala.reflect.runtime.universe
      val m     = ru.runtimeMirror(getClass.getClassLoader)
      val selfM = m.reflect(self)
      val subM  = m.reflect(sub)

      val map = subM.symbol.info.members.collect {
        case mem if !mem.isMethod && mem.isTerm => {
          (
            mem.name.toString.trim(),
            subM.reflectField(
              mem.asTerm
            ).get.asInstanceOf[Data]
          )
        }
      }.toMap.withDefaultValue(null)

      selfM.symbol.info.members.collect {
        case mem if !mem.isMethod && mem.isTerm => {
          (
            mem.name.toString.trim(),
            selfM.reflectField(
              mem.asTerm
            ).get.asInstanceOf[Data]
          )
        }
      }.map { case (name, el) =>
        val target = map(name)

        if (target != null && DataMirror.checkTypeEquivalence(el, target)) {
          val dir = DataMirror.specifiedDirectionOf(el)
          if (dir == SpecifiedDirection.Input) {
            if (debug) println(
              f"self.$name%-16s >>  sub.$name%-16s"
            )
            target := el
          } else if (dir == SpecifiedDirection.Output) {
            if (debug) println(
              f" sub.$name%-16s << self.$name%-16s"
            )
            el := target
          }
        }
      }
    }
  }
}
