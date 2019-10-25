package memnets.model

import java.util

import scala.collection.JavaConverters._

trait F extends Element {
  def act: Double
  def eval(te: Tick): Double
  def inputs: Iterable[Y]
  def inputs_=(ins: Iterable[Y]): Unit
  def owner: Y
  def scale: Option[Float]
  def scale_=(s: Double): Unit
  // Java
  def getScale: Double = scale.getOrElseP(YRange.scaleF)
  def setScale(f: Double): Unit = { scale = f }
  def getInputs: util.Collection[Y] = inputs.asJavaCollection
  def setInputs(ins: util.Collection[Y]): Unit = { inputs = ins.asScala }
}
