package memnets.model

/** trait used for TimeChart */
trait Trackable {
  def act: Double
  def isSpiking: Boolean
  def ui: TrackableUI
}
