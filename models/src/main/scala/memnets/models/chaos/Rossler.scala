package memnets.models.chaos

import memnets.model._

class Rossler(val tau: Double = 30.0)(implicit mn: DynamicSystem) extends ElementBase with Layout {

  val a = Param("a", max = 1.0, init = 0.2)
  val c = Param("c", max = -10.0, init = -5.7)
  val b = 0.2

  val x = Y("x", tau = tau)
  val y = Y("y", tau = tau)
  val z = Y("z", tau = tau)

  y --> y tie = a
  z --> z tie = c

  y --> x w = -1
  z --> x w = -1
  x --> y

  z.f("z * x", z, x) { t =>
    z * x
  }
  z.threshold = -b

  def layout(): Unit = {
    z.ui.loc = loc
    x.ui.loc = z.ui.loc.down(180).left()
    y.ui.loc = x.ui.loc.right(200)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }

  mn.elements += this
}
