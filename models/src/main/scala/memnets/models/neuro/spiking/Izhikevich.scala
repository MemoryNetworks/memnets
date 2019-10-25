package memnets.models.neuro.spiking

import memnets.model.Activation._
import memnets.model._

/** NOTE: V is the spiking variable  */
class Izhikevich(val tau: Double = 30.0)(implicit mn: DynamicSystem) {
  val a = Param("a", max = -0.10, init = -0.02)
  val b = Param("b", max = 0.6, init = 0.2)
  val c = Param("c", max = -85, init = -65.0)
  val d = Param("d", max = 5.0, init = 2.0)

  val v = Y("v", tau = tau, decay = 5.0, threshold = -140.0, act = Spike, scale = 30.0)
  v.spike = 30.0
  v.f(".04v^2", v) { t =>
    0.04 * v * v
  }

  val u = Y("u", tau = tau, scale = 30.0)
  u.f("-ab*v", v) { t =>
    -a.getValue * b.getValue * v
  }
  u --> u tie = a
  u --> v w = -1.0

  def spikeReset(): Unit = {
    v.update(c.getValue)
    u.update(u + d.getValue)
  }
  def reset(): Unit = {
    v.update(c.getValue)
    u.update(u + d.getValue)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
}
