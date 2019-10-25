package memnets.models.control

import memnets.model._

class PD(tau: Double = 10.0)(implicit mn: DynamicSystem) extends ElementBase with Layout {
  val tgt = Y("tgt", decay = -1.0, tau = 4.0)
  //    val out = Y("out", tau = 6*tau)
  val out = Y("out", tau = 60.0)
  val error = Y("error", decay = -1.0, tau = tau)
  val errorD = Y("error'", decay = -1.0, tau = tau)
  val Kp = Param("Kp", max = 50.0, init = 42.0)
  val Kd = Param("Kd", max = -50.0, init = -30.0)

  tgt --> error
  out --> error w = -1.0
  error --> out tie = Kp
  error --> errorD
  errorD --> out tie = Kd

  def layout(): Unit = {
    error.ui.loc = loc.up(60)
    errorD.ui.loc = error.ui.loc.right()
    Seq(tgt, out).center(loc.down(200), 120)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
  name = "PD Controller"
  mn.elements += this
}
