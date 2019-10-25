package memnets.models.chaos

import memnets.model.GREEK._
import memnets.model._

class Lorentz(val tau: Double = 75.0)(implicit mn: DynamicSystem) extends ElementBase with Layout {

  val sigma = Param(SIGMA, max = 20.0, init = 10.0)
  val sigmaNeg = Param(s"-${SIGMA}", max = -20.0, init = -10.0, system = true)
  sigma ==> { w =>
    sigmaNeg.value = -w
  }

  val beta = Param(BETA, max = -6.0, init = -8.0 / 3.0)
  val rho = Param(RHO, max = 40, init = 28)

  val x = Y("x", tau = tau, scale = 15.0)
  val y = Y("y", tau = tau, scale = 15.0, decay = -1.0)
  val z = Y("z", tau = tau, scale = 15.0)
  y --> x tie = sigma
  x --> x tie = sigmaNeg
  x --> y tie = rho
  y.func("x * z", inputs = Seq(x, z), scale = 150.0) { t =>
    -x * z
  }
  z.func("x * y", inputs = Seq(x, y), scale = 150.0) { t =>
    x * y
  }
  z --> z tie = beta

  def layout(): Unit = {
    x.ui.loc = loc.down(200)
    y.ui.loc = x.ui.loc.up(200).right(120)
    z.ui.loc = x.ui.loc.up(200).left(120)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }

  name = "Lorentz"
  mn.elements += this
}
