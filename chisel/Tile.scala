package hammer

import chisel3._
import chisel3.util._

class TileSeq[T](val xLen: Int, val yLen: Int)(gen: => T) {
  val inner: Seq[Seq[T]] = Seq.fill(xLen)(Seq.fill(yLen)(gen))

  def apply(x: Int, y: Int) = inner(x)(y)
  def x(idx:   Int)         = inner(if (idx >= 0) idx else xLen - idx)
  def y(idx: Int) = inner.map(i => i(if (idx >= 0) idx else yLen + idx))
  def foreach(f: (T, Int, Int) => Unit): Unit = inner.zipWithIndex
    .flatMap { case (seq, row) =>
      seq.zipWithIndex.map { case (el, col) => (el, row, col) }
    }.map { case (el, row, col) => f(el, row, col) }
}

object TileSeq {
  def apply[T](xLen: Int, yLen: Int)(gen: => T): TileSeq[T] =
    new TileSeq(xLen, yLen)(gen)
}

class Tile[T <: Data](xLen: Int, yLen: Int, gen: => T)
    extends Bundle {
  val bits = Vec(xLen, Vec(yLen, gen))

  def apply(x: Int, y: Int) = bits(x)(y)
  def x(idx:   Int)         = bits(if (idx >= 0) idx else xLen - idx)
  def y(idx: Int) = VecInit(bits.map(i => i(if (idx >= 0) idx else yLen + idx)))
}

object Tile {
  def apply[T <: Data](xLen: Int, yLen: Int, gen: T) =
    new Tile(xLen, yLen, gen)
}
