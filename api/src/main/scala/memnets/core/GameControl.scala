package memnets.core

/**
 * NOTE: ui.Skin does not directly support controls
 * to see how a subclass can use, see fx.SkinFX
 */
trait GameControl {
  def reset(): Unit
}
