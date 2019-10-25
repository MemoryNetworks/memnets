package memnets.core

trait GameControlSkin[T <: AnyRef] {
  def control(gc: GameControl): Option[T]
}
