package hammer

import chisel3._
import circt.stage.ChiselStage
import scala.language.existentials

class ExportedModule(
    gen:  => Module,
    name: String = "RegWrapper"
)(useOutputBuffer: Boolean = true) extends Module {

  override def desiredName: String = name

  val inner = Module(gen)

  // Reflect to get 'io' field
  // Scala why ur reflect is SO COMPLEX?
  val ru    = scala.reflect.runtime.universe
  val m     = ru.runtimeMirror(getClass.getClassLoader)
  val im    = m.reflect(inner)
  val field = im.reflectField(
    im.symbol.info.member(ru.TermName("io")).asTerm.accessed.asTerm
  )
  require(field != null, "Exported Module must have a 'io' field!")

  val innerIO    = field.get.asInstanceOf[Bundle]
  val bufferedIO = if (useOutputBuffer) RegOut(innerIO) else innerIO
  val io         = IO(chiselTypeOf(innerIO))
  io <> bufferedIO
}

object Export {

  /**
    * Export a module to SystemVerilog
    *
    * @param gen The module to export
    * @param path The path to save source files. The final path will be `{Project Root}/build/{path}`
    * @param firOpts Firrtl options
    * @param useOutputBuffer If true, all the outputs will be wrapped with registers, Useful if you're going Synthesis the design for timing reports
    */
  def apply(
      gen:             => Module,
      path:            String,
      firOpts:         Array[String] = Array(),
      useOutputBuffer: Boolean = true
  ): Unit =
    ChiselStage.emitSystemVerilogFile(
      new ExportedModule(gen, "Top")(useOutputBuffer),
      firtoolOpts = Array(
        "-disable-all-randomization",
        "-strip-debug-info",
        "-default-layer-specialization=enable"
      ) ++ firOpts,
      args = Array("--target-dir", "build/" + path)
    )
}
