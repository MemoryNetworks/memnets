package memnets.ui

import memnets.model._

trait Scene3D extends Element {
  def cameraPos: Pt3D
  def moveCamera(x: Double, y: Double): Unit
  def moveCamera(x: Double, y: Double, z: Double): Unit
}

trait AutoCamera {
  def tick(te: Tick, scene: Scene3D): Unit
}
object NullAutoCamera extends AutoCamera {
  def tick(te: Tick, scene: Scene3D): Unit = {}
}

trait Pt3D {
  def x: Double
  def y: Double
  def z: Double
}
