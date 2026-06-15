package hammer

import _root_.circt.stage._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._

object Export {
  def apply(
      gen:     => Module,
      args:    Array[String],
      firOpts: Array[String] = Array(),
  ) = {

    var realArgs = args
    if(!realArgs.contains("--target")) realArgs ++= Array(
      "--target",
      "systemverilog",
    )
    val firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable",
    ) ++ firOpts

    (new ChiselStage).execute(
      realArgs,
      Seq(ChiselGeneratorAnnotation(() =>
        gen,
      )) ++
        firtoolOpts.map(FirtoolOption(_)),
    )
  }
}
