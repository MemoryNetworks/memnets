package memnets.fx.app

import javafx.application.Application
import memnets.core._

object SingletonAppFX {
  private var _startUpBuilder: ModelBuilder = _
  private var _config: ModelConfig = _

  /** helper for Java */
  def demoJ(builder: BldType, args: Array[String] = Array()): Unit = {
    demo(builder, args = args)
  }
  def demo(builder: BldType, config: ModelConfig = new ModelConfig, args: Array[String] = Array()): Unit = {
    _startUpBuilder = builder
    _config = config
    main(args)
  }
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SingletonAppFX], args: _*)
  }
}
final private class SingletonAppFX extends AppBaseFX {
  def startUpBuilder: BldType = SingletonAppFX._startUpBuilder
  override def config: ModelConfig = SingletonAppFX._config
}
class DemoFX(bld: ModelBuilder, config: ModelConfig = new ModelConfig) {
  def main(args: Array[String]): Unit = {
    SingletonAppFX.demo(bld, config, args)
  }
}
