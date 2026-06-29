package hammer

import chisel3._
import chisel3.util._
import scala.collection.mutable

class PseudoLruSelectorIO(width: Int) extends Bundle {
  val hitValid = Input(Bool())
  val hitIndex = Input(UInt(width.W))

  val replaceValid = Input(Bool())
  val replaceIndex = Output(UInt(width.W))
}

class PseudoLruSelector(width: Int) extends Module {
  require(
    isPow2(width),
    "Currently PseudoLruSelector supports width = 2 ^ n only!",
  )

  val io = IO(new PseudoLruSelectorIO(width))

  val layer = log2Ceil(width)
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

    replacePath(i).withIndex(width).map(data =>
      data.bits := data.index === pointers(i - 1),
    )

    hitPath(i).withIndex(width).map(data =>
      data.bits := data.index === io.hitIndex.head(i),
    )
  }

  for (i <- 0 until layer) {
    nodes(i).zip(replacePath(i)).zip(hitPath(i)).map { case ((n, w), h) =>
      n := MuxIf(
        (h && io.hitValid)   -> !io.hitIndex(width - 1 - i),
        (w && io.replaceValid) -> !n,
      )(n)
    }
  }

  io.replaceIndex := pointers(layer - 1)
}
