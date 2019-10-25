package memnets.core

import memnets.model.{Tick, TickListener}

trait AppUI extends EngineListener with TickListener {

  /** load model libraries and other resources*/
  def loadResources(): Unit
  /** assumption that loadResources has been called before this */
  def startUpBuilder: BldType
  def config: ModelConfig
  def runOnUI[R](op: => R): Unit = { op }
  def task[T](msg: String)(et: EngineTask[T]): Unit = {
    val t = et.call()
    et.onCompleted(t)
  }
  def tick(te: Tick): Unit = {}
}

/** don't put into core.package b/c can't access from java */
object NullAppUI extends AppUI {
  val config = ModelConfig()
  def loadResources(): Unit = {}
  def startUpBuilder: BldType = ModelBuilder("NullModel") { b => }
  def process(ee: EngineEvent): Unit = {}
}
