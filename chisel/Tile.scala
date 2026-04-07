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
  def mapBundle[B <: Data](f: (T, Int, Int) => B): Tile[B] =
    TileInit(xLen, yLen, (x, y) => f(apply(x, y), x, y))

  def mapX[B](f: (Seq[T], Int) => B): Seq[B] =
    inner.zipWithIndex.map { case (s, x) => f(s, x) }
  def mapY[B](f: (Seq[T], Int) => B): Seq[B] =
    Seq.tabulate(yLen)(y => f(Seq.tabulate(xLen)(x => apply(x, y)), y))

  def zip[B](other: TileSeq[B]): TileSeq[(T, B)] = {
    if (other.xLen != xLen || other.yLen != yLen) throw new RuntimeException(
      s"The size of $this ($xLen, $yLen) and $other (${other.xLen}, ${other.yLen}) is not equal"
    )
    return TileSeq(xLen, yLen, (x, y) => (apply(x, y), other(x, y)))
  }
}

object TileSeq {
  def apply[T](xLen: Int, yLen: Int, gen: (Int, Int) => T): TileSeq[T] =
    new TileSeq(xLen, yLen)(gen)

  def apply[T](xLen: Int, yLen: Int, gen: => T): TileSeq[T] =
    new TileSeq(xLen, yLen)((_, _) => gen)
}

class Tile[T <: Data](val xLen: Int, val yLen: Int)(
    gen:    (Int, Int) => T,
    isType: Boolean
) extends Bundle {
  val bits = if (isType)
    Vec(xLen, Vec(yLen, gen(0, 0)))
  else VecInit(Seq.tabulate(xLen)(x =>
    VecInit(Seq.tabulate(yLen)(y => prefix(s"pos_${x}_$y")(gen(x, y))))
  ))

  def apply(x: Int, y: Int): T      = bits(x)(y)
  def x(idx:   Int):         Vec[T] = bits(if (idx >= 0) idx else xLen - idx)
  def y(idx: Int):           Vec[T] =
    VecInit(bits.map(i => i(if (idx >= 0) idx else yLen + idx)))
  def map[B](f: (T, Int, Int) => B): TileSeq[B] =
    TileSeq(xLen, yLen, (x, y) => f(apply(x, y), x, y))

  def mapX[B <: Data](f: (Seq[T], Int) => B): Seq[B] =
    bits.zipWithIndex.map { case (s, x) => f(s, x) }
  def mapY[B <: Data](f: (Seq[T], Int) => B): Seq[B] =
    Seq.tabulate(yLen)(y => f(Seq.tabulate(xLen)(x => apply(x, y)), y))

  def zip[B <: Data](other: Tile[B]): TileSeq[(T, B)] = {
    if (other.xLen != xLen || other.yLen != yLen) throw new RuntimeException(
      s"The size of $this ($xLen, $yLen) and $other (${other.xLen}, ${other.yLen}) is not equal"
    )
    return TileSeq(xLen, yLen, (x, y) => (apply(x, y), other(x, y)))
  }

  def elemOp[B <: Data, R <: Data](other: Tile[B])(op: (T, B) => R): Tile[R] = {
    if (other.xLen != xLen || other.yLen != yLen) throw new RuntimeException(
      s"The size of $this ($xLen, $yLen) and $other (${other.xLen}, ${other.yLen}) is not equal"
    )
    val tile =
      Wire(Tile(xLen, yLen, chiselTypeOf(op(apply(0, 0), other(0, 0)))))
    tile.map((el, x, y) => el := op(apply(x, y), other(x, y)))
    return tile
  }
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
