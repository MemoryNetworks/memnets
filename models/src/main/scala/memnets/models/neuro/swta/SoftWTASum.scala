package memnets.models.neuro.swta

import memnets.model.Activation._
import memnets.model._

class SoftWTASum(val n: Int, val tau: Double = 10.0)(implicit sys: DynamicSystem)
    extends SoftWTASparse
    with AbstractSoftWTASum[Y] {
  val e2eT = Param("e2e", 0.6, 0.3)
  val e2iT = Param("e2i", 1.0, 0.2)
  val i2eT = Param("i2e", -10.0, -5.0)
  val inhib = Y("inh", decay = -1.0, tau = 4.0, act = Relu, threshold = 1.0, scale = 2.0)
  val sum = Y(GREEK.SIGMA_UPPERCASE, decay = -1.0, tau = 1.0, act = Relu, scale = 2.0)
  val sum2inh: E = sum --> inhib
  val excites = IndexedSeq.tabulate(n) { i =>
    val x = Y(s"ex$i", tau = tau, act = Relu, threshold = 1.0)
    //    x.showText = true
    x --> x tie = e2eT
    inhib --> x tie = i2eT
    x --> sum tie = e2iT
    x
  }
  override def layoutHelper(
      ctr: Loc = loc,
      onBottom: Boolean = true,
      spacing: Double = (Display.width - 400.0) / size,
      vspacing: Double = 140.0): Unit = {
    super.layoutHelper(ctr, onBottom, spacing, vspacing)
    sum.ui.loc = inhib.ui.loc.right(50)
  }
  name = "sWTAConv"
  sys.elements += this
}
