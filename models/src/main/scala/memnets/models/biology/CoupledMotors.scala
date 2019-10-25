package memnets.models.biology

import memnets.model._

class CoupledMotors(tau: Double = 90.0)(implicit mn: DynamicSystem) extends ElementBase with Layout {
  val e = Y("Excite")
  val e2 = Y("Excite")
  val i = Y("Inhib", tau = tau) // decay = -1.0 not stable
  val i2 = Y("Inhib", tau = tau)

  val eDecay = Param("eDecay", 0.5, 0.3)
  val iDecay = Param("iDecay", -1.0, -0.9)
  val e2i = Param("e2i", 2.0, 1.0)
  val i2e = Param("i2e", max = -10.0, -5.0)
  val i2eC = Param("i2eC", max = -2.0, -0.5)

  e --> e tie = eDecay
  e2 --> e2 tie = eDecay
  e --> i tie = e2i
  e2 --> i2 tie = e2i
  i --> e tie = i2e
  i2 --> e2 tie = i2e
  i --> e2 tie = i2eC
  i2 --> e tie = i2eC
  i --> i tie = iDecay
  i2 --> i2 tie = iDecay

  def layout(): Unit = {
    e.ui.loc = loc
    i.ui.loc = e.ui.loc.right(160)
    e2.ui.loc = e.ui.loc.down(120)
    i2.ui.loc = e2.ui.loc.right(160)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
  mn.elements += this
}
