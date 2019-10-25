package memnets.core

import memnets.model._
import memnets.ui._

trait ModelUI extends TickListener {
  def add(elem: Element): Unit
  def defaultSkin: SkinType
  def defaultSkins: Iterable[SkinType] = List(defaultSkin)
  def dirty: Boolean
  def engine: Engine
  def engine_=(engine: Engine): Unit
  def flashTitle(): Unit
  def init(): Unit = {}
  def isValid(skin: SkinType): Boolean
  def system: DynamicSystem = model.system
  def model: BuiltModel
  def model_=(m: BuiltModel): Unit
  def playGameIntro(): Unit
  def playGameOver(win: Boolean): Unit
  def publish(ee: EngineEvent): Unit = if (engine != null) engine.publish(ee)
  def rebuild(): Unit
  def remove(o: Element): Unit
  def reset(fullReset: Boolean): Unit
  def setFPS(fps: Int): Unit
  def skin: SkinType
  def skin_=(f: SkinType): Unit

  /** immediately stop all animations.  the builder/model is being destroyed */
  def stopAllAnimations(): Unit
  def updateScore(): Unit

  /** should wait for rebuild to make any changes to UI */
  def trial: Trial
  def trial_=(t: Trial): Unit

  def getModel: BuiltModel = model
  def setModel(m: BuiltModel): Unit = { model = m }
  def getSkin: SkinType = skin
  def setSkin(s: SkinType): Unit = { skin = s }
  def getTrial: Trial = trial
  def setTrial(t: Trial): Unit = { trial = t }
}
