package hammer

import _root_.circt.stage._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._

object Export {

  /**
    * Export a Module to specific target
    *
    * @param gen The module to be exported
    * @param args The args to be passed to `firtool`
    * @param firOpts The options for `firtool` to adjust its product
    */
  def apply(
      gen:     => Module,
      args:    Array[String],
      firOpts: Array[String] = Array(),
  ): Unit = {

    var realArgs = args
    if (!realArgs.contains("--target")) realArgs ++= Array(
      "--target",
      "systemverilog",
    )
    val firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable",
      "-O=release",
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
