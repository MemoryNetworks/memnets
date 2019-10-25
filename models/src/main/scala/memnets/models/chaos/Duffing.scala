package memnets.models.chaos

import memnets.model._

class Duffing(val tau: Double = 30.0)(implicit mn: DynamicSystem) {
  import GREEK._
  val alpha = Param(ALPHA, max = 1.5, init = 1)
  val beta = Param(BETA, max = 5, init = 1)
  val delta = Param(DELTA, max = -0.5, init = -0.2)

  val x = Y("x", tau = tau)
  val dx = Y("x'", tau = tau)
  // p. 28-30 Chaos
  dx --> x tie = alpha
  dx --> dx tie = delta
  dx.f(s"-${beta.name} * x^3", x) { t =>
    -beta.getValue * x * x * x
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
}
