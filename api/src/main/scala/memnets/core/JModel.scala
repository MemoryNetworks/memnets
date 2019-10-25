package memnets.core

import java.util.function.Consumer

import memnets.core.{Trial => TrialBld}
import memnets.model.{
  Activation,
  Layer => Layer2,
  Loc => Loc2,
  Param => Param2,
  Sin => Sin2,
  Step => Step2,
  Trial => Trial2,
  Y => Y2,
  _
}
import memnets.ui.{Phase => Phase2, Skin => Skin2, _}

class JModel(val mod: Model) extends Dsl {
  type YType = Y2
  import mod._
  def sizeHint(default: Int, min: Int) = mod.sizeHint(default, min)
  def sizeHint(default: Int) = mod.sizeHint(default)
  def Y(name: String, decay: Double, tau: Double, activation: Activation): Y2 =
    Y2(name = name, decay = decay, tau = tau, act = activation)
  def Y(name: String, decay: Double): Y2 = Y2(name = name, decay = decay)
  def Y(name: String): Y2 = Y2(name = name)
  def Y(decay: Double): Y2 = Y2(name = "", decay = decay)
  def Y(): Y2 = Y2("")
  def Layer(n: Int, decay: Double): Layer2 = Layer2(n = n, decay = decay)
  def Layer(n: Int): Layer2 = Layer2(n)
  def Layer(n: Int, name: String): Layer2 = Layer2(n = n, name = name)
  def Layer(n: Int, name: String, decay: Double): Layer2 = Layer2(n = n, name = name, decay = decay)

  def Param(name: String, max: Double, init: Double): Param2 = Param2(name = name, max = max, init = init)
  def Param(name: String, max: Double, init: Double, min: Double): Param2 =
    Param2(name = name, max = max, init = init, min = min)

  def Trial(time: Int, name: String): Trial2 = TrialBld(time = time, name = name)
  def Trial(time: Int): Trial2 = TrialBld(time = time)
  def Trial(name: String): Trial2 = TrialBld(name = name)
  def Trial(): Trial2 = TrialBld()
  def toSec(i: Int): Int = i * 60
  def toMin(i: Int): Int = i * 3600
  def toMin(i: Double): Int = (i * 3600.0).toInt

  def Step(y: Yb): Signal = Step2(y = y)
  def Step(y: Yb, on: Int, scale: Double): Signal = Step2(y = y, on = on, scale = scale)
  def Step(y: Yb, on: Int, dur: Int, scale: Double): Signal = Step2(y = y, on = on, dur = dur, scale = scale)

  def Sin(y: Yb, on: Int): Signal = Sin2(y, on)
  def Sin(y: Yb, on: Int, period: Double): Signal = Sin2(y, on, period)
  def Sin(y: Yb, on: Int, period: Double, phase: Double, scale: Double): Signal =
    Sin2(y, on = on, period = period, phase = phase, scale = scale)

  def Phase(x: Yb, y: Yb): Phase2D = Phase2(x, y)
  def Phase(x: Yb, y: Yb, scale: Double): Phase2D = Phase2(x, y, scale = scale)

  def Loc(): Loc2 = Loc2()
  def Loc(x: Double, y: Double): Loc2 = Loc2(x, y)
  def Skin(): SkinType = Skin2()
  def Skin(init: Consumer[SkinType]): SkinType = Skin2(init = init.accept)
  def track(tr: Trackable): Unit = mod.tracked += tr
  import collection.JavaConverters._
  def track(tr: java.util.Collection[Trackable]): Unit = mod.tracked ++= tr.asScala

  def getLogger = mod.getLogger
  // def getModel = model
  def System = mod.system
  def getSystem = mod.system

  def getSkin() = skins.headOption.getOrElseP(NullSkin)
  def setSkin(skin: SkinType): Unit = addSkin(skin)
  def addSkin(skin: SkinType): Unit = mod.skin = skin
}
