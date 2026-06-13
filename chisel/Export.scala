package hammer

import _root_.circt.stage.ChiselStage
import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._

object Export {
  def apply(
      gen:     => Module,
      args:    Array[String],
      firOpts: Array[String] = Array(),
  ) = {

    val a = Array(
      "--target",
      "systemverilog",
    ) ++ args

    val firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable",
    ) ++ firOpts

    (new ChiselStage).execute(
      a,
      Seq(ChiselGeneratorAnnotation(() =>
        gen,
      )) ++
        firtoolOpts.map(FirtoolOption(_)),
    )
  }
}
