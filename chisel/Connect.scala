package hammer

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import io.AnsiColor._

object Connect {
  def apply(
      src:      Data,
      dst:      Data,
      srcToDst: (Data, Data) => Unit = (s, d) => d := s,
      dstToSrc: (Data, Data) => Unit = (s, d) => s := d,
      flipped:  Boolean = false
  ): Unit = {
    DataMirror.requireTypeEquivalent(
      src,
      dst,
      s"Unable to connect signal '${src}' -> '${dst}': They're of different types"
    )

    if (DataMirror.specifiedDirectionOf(dst) == SpecifiedDirection.Output) {
      if (flipped) dstToSrc(src, dst)
      else srcToDst(src, dst)
    } else if (
      DataMirror.specifiedDirectionOf(dst) == SpecifiedDirection.Input
    ) {
      if (flipped) srcToDst(src, dst)
      else dstToSrc(src, dst)
    } else if (src.isInstanceOf[Aggregate]) {
      val srcAgg = src.asInstanceOf[Aggregate]
      val dstAgg = dst.asInstanceOf[Aggregate]

      srcAgg.getElements.zip(dstAgg.getElements).map { case (s, d) =>
        Connect(
          s,
          d,
          srcToDst,
          dstToSrc,
          flipped ^ (DataMirror.specifiedDirectionOf(
            src
          ) == SpecifiedDirection.Flip)
        )
      }
    } else {
      println(
        s"${RED}Unable to connect signal '$src' -> '$dst': Cannot determine direction$RESET"
      )

    }
  }
}
