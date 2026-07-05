package hammer

import chisel3._
import chisel3.util._
import scala.collection.mutable

class PseudoLruSelectorIO(size: Int) extends Bundle {
  val hitValid = Input(Bool())
  val hitIndex = Input(UInt(size.W))

  val replaceValid = Input(Bool())
  val replaceIndex = Output(UInt(size.W))
}

/**
  * The Pseudo Least Recently Used resolver
  * 
  * This module functions as a pointer to the plru target that should be replaced
  * 
  * **Signals**
  * - `hitValid`: whether a read/write hit happens
  * - `hitIndex`: the index of the hit element
  * - `replaceValid`: whether a replace will happen
  * - `replaceIndex`: the index to be replaced
  *
  * @param size The element count of the table
  */
class PseudoLruSelector(size: Int) extends Module {
  require(
    isPow2(size),
    "Currently PseudoLruSelector supports width = 2 ^ n only!",
  )

  val width = log2Ceil(size)

  val io = IO(new PseudoLruSelectorIO(width))

  val layer = log2Ceil(size)
  val nodes =
    Seq.tabulate(layer)(i => RegZero(Vec(Pow2(i), Bool())))

  val replacePath =
    Seq.tabulate(layer)(i => WireZero(Vec(Pow2(i), Bool())))
  replacePath(0)(0) := true.B

  val hitPath =
    Seq.tabulate(layer)(i => WireZero(Vec(Pow2(i), Bool())))
  hitPath(0)(0) := true.B

  var pointers: mutable.Seq[UInt] = mutable.Seq.fill(layer)(null)
  pointers(0) = nodes(0)(0)

  for (i <- 1 until layer) {
    pointers(i) = pointers(i - 1) ## nodes(i)(pointers(i - 1))

    replacePath(i).zipWithIndex.map { case (data, idx) =>
      data := idx.U === pointers(i - 1)
    }

    hitPath(i).zipWithIndex.map { case (data, idx) =>
      data := idx.U === io.hitIndex.head(i)
    }
  }

  for (i <- 0 until layer) {
    nodes(i).zip(replacePath(i)).zip(hitPath(i)).map {
      case ((node, replace), hit) =>
        node := MuxIf(
          (hit && io.hitValid)         -> !io.hitIndex(width - 1 - i),
          (replace && io.replaceValid) -> !node,
        )(node)
    }
  }

  io.replaceIndex := pointers(layer - 1)
}
