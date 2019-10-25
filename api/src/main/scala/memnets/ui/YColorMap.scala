package memnets.ui

import memnets.model._

trait YColorMap[T <: AnyRef] {
  def trackColor(trackable: Trackable): T = trackable match {
    case y: Y =>
      apply(y)
    case yb: Yb =>
      apply(yb)
    case sig: Signal =>
      signalColor(trackColor(sig.tgt))
  }
  def signalColor(t: T): T = t
  def apply(y: Y): T = apply(y.layer, y.id)

  /**
   * NOTE: if store off Y as Yb ref, compiler will call this, which does not check unsupported Yb custom color
   * if want to be safe, call trackColor for signal.tgt or phase.x,y,z
   */
  def apply(yb: Yb): T = apply(yb.layer, yb.id)
  def apply(layer: LayerLike, i: Int): T = apply(i, layer.length)
  def apply(layer: LayerLike): T = apply(layer.id, layer.system.layers.size)
  def apply(i: Int, length: Int): T
}

trait YGradientMap[T <: AnyRef] extends YColorMap[T] {
  def hints: GradientHints
  def hints_=(hints: GradientHints): Unit
}

trait YGradientMapFactory[T <: AnyRef] {
  def create(hints: GradientHints): YGradientMap[T]
}
