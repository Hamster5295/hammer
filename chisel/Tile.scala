package hammer

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.experimental.prefix
import chisel3.util._

class TileSeq[T](val xLen: Int, val yLen: Int)(gen: (Int, Int) => T) {
  val inner: Seq[Seq[T]] = Seq.tabulate(xLen)(x =>
    Seq.tabulate(yLen)(y => prefix(s"pos_${x}_$y")(gen(x, y)))
  )

  def apply(x: Int, y: Int) = inner(x)(y)
  def x(idx:   Int) = inner(if (idx >= 0) idx else xLen - idx)
  def y(idx:   Int) = inner.map(i => i(if (idx >= 0) idx else yLen + idx))
  def map[B](f: (T, Int, Int) => B): TileSeq[B] =
    TileSeq(xLen, yLen, (x, y) => f(apply(x, y), x, y))
}

object TileSeq {
  def apply[T](xLen: Int, yLen: Int, gen: (Int, Int) => T): TileSeq[T] =
    new TileSeq(xLen, yLen)(gen)

  def apply[T](xLen: Int, yLen: Int, gen: => T): TileSeq[T] =
    new TileSeq(xLen, yLen)((_, _) => gen)

}

class Tile[T <: Data](xLen: Int, yLen: Int)(gen: (Int, Int) => T, isType: Boolean)
    extends Bundle {
  val bits = if (isType)
    Vec(xLen, Vec(yLen, gen(0, 0)))
  else VecInit(Seq.tabulate(xLen)(x =>
    VecInit(Seq.tabulate(yLen)(y => prefix(s"pos_${x}_$y")(gen(x, y))))
  ))

  def apply(x: Int, y: Int): T      = bits(x)(y)
  def x(idx:   Int):         Vec[T] = bits(if (idx >= 0) idx else xLen - idx)
  def y(idx:   Int):         Vec[T] = VecInit(bits.map(i => i(if (idx >= 0) idx else yLen + idx)))
  def map[B <: Data](f: (T, Int, Int) => B): Tile[B] =
    TileInit(xLen, yLen, (x, y) => f(apply(x, y), x, y))
}

object Tile {
  def apply[T <: Data](
      xLen: Int,
      yLen: Int,
      gen:  => T
  ) =
    new Tile(xLen, yLen)((_, _) => gen, true)
}

object TileInit {
  def apply[T <: Data](
      xLen: Int,
      yLen: Int,
      gen:  => T
  ) =
    new Tile(xLen, yLen)((_, _) => gen, false)

  def apply[T <: Data](
      xLen: Int,
      yLen: Int,
      gen:  (Int, Int) => T
  ) =
    new Tile(xLen, yLen)(gen, false)
}