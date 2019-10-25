package memnets.models.neuro.spiking

import memnets.model.Activation._
import memnets.model._

class ResFire(val spike: Double = 4.0, val tau: Double = 30.0)(implicit mn: DynamicSystem) extends Linkable {
  val na = Y("Na", tau = 3.0, act = Spike)
  na.spike = spike
  val k = Y("K", tau = tau)
  k.ui.viz = Viz.Fade
  // period  related to ratio of Na/K tau
  na --> na tie = Param("na --> na", max = 0.2, init = -0.5, min = -1.0) // > -1.0 causes burst
  na --> k
  k --> k tie = Param("k --> k", max = -1.0, init = -0.2)
  k --> na tie = Param("k --> na", max = -2.0, init = -1.3)

  override def src: Y = na

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
}
