package memnets.models.neuro.swta

import memnets.model._

class SoftWTA(
    n: Int,
    tau: Double = DynamicSystem.TAU_DEFAULT
)(implicit sys: DynamicSystem)
    extends SoftWTASparse
    with Logging {

  val e2eT = Param("e2e", 0.8, 0.2)
  val e2iT = Param("e2i", 1.0, 0.2)
  val i2eT = Param("i2e", -10.0, -5.0)
  val inhib = Y("inhib", decay = -1.0, act = Activation.Relu, threshold = 1.0, scale = 1.0, tau = tau)
  val excites: IndexedSeq[Y] = IndexedSeq.tabulate(n) { i =>
    val x = Y(s"exc$i", act = Activation.Relu, tau = tau)
    x --> x tie = e2eT
    inhib --> x tie = i2eT
    x --> inhib tie = e2iT
    x
  }
  inhib.ui.color = Colorf.INHIB
  inhib.ui.loc = Loc()
  inhib.noiseScale = 0.0

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
  // Element
  name = "sWTA"
  sys.elements += this
}
