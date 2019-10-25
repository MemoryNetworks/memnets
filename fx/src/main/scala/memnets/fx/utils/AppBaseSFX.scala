package memnets.fx.utils

import javafx.application.Application
import scalafx.stage.Stage

import scala.collection.mutable.ListBuffer

/**
 * need app to derive from javafx.application.Application for IntelliJ JavaFX app artifacts
 * NOTE: purposefully separating Base to force subClassInits initialization before calls to delayedInit...
 */
abstract class ApplicationSFX extends javafx.application.Application {
  private var _stage: Stage = null
  protected var _splash: Option[Splash] = None
  private var _parameters: Application.Parameters = _
  private[utils] val _subClassInits = ListBuffer[() => Unit]()
  protected var _showAtStart = true

  def parameters = _parameters
  def stage = _stage
  override def init(): Unit = {
    super.init()
    _parameters = getParameters
  }
  override def start(primary: javafx.stage.Stage): Unit = {
    _stage = new Stage(primary)
    _splash = splashInit()
    for (init <- _subClassInits)
      init()
    if (_showAtStart)
      _stage.show()
  }
  protected def splashInit(): Option[Splash] = None
}
abstract class AppBaseSFX extends ApplicationSFX with DelayedInit {
  def delayedInit(body: => Unit): Unit = { _subClassInits += (() => body) }
}

class TestAppFX extends AppBaseSFX {}
trait Splash {
  def close(): Unit
}
