package memnets.models.neuro.swta

import memnets.model._

class SoftWTAGrid(
    override val rows: Int,
    override val cols: Int,
    override val tau: Double = DynamicSystem.TAU_DEFAULT,
    val unitViz: Viz = Viz.Default
)(implicit sys: DynamicSystem)
    extends YGrid(rows, cols, tau) {

  val max = 1.5
  val pInit = 0.0
  val rightTie = Param("right", max, pInit)
  val leftTie = Param("left", max, pInit)
  val upTie = Param("up", max, pInit)
  val downTie = Param("down", max, pInit)
  val boostTie = Param("boost", -0.5, pInit)

  val inhib = Y("inh", decay = -1.0, tau = 3.0, threshold = 0.4, act = Activation.Relu)
  inhib.f("boost") { t =>
    boostTie.getValue
  }
  inhib.ui.viz = Viz.Hide
  inhib.ui.color = Colorf.INHIB

  val e2i = Param("e2i", 1, 0.2)
  val i2e = Param("i2e", -10, -5)

  override def create(r: Int, c: Int): Y = {
    val x = Y(" ", decay = 0.3, tau = tau, act = Activation.Relu)
    x.ui.viz = unitViz
    x.ui.loc = Loc(140 + c * 36, 50 + r * 36)
    x
  }
  inhib.ui.loc = Loc(apply(0, cols / 2).ui.loc.x, 35.0)

  // link to inhib
  for ((i, j, x) <- units) {
    x --> inhib tie = e2i
    inhib --> x tie = i2e
  }
  // horizontal tied
  for {
    row <- rowData
    pair <- row.sliding(2)
    x = pair(0)
    x2 = pair(1)
  } {
    x --> x2 tie = rightTie
    x2 --> x tie = leftTie
  }
  // vertical tied
  for {
    rowPair <- rowData.sliding(2)
    row = rowPair(0)
    row2 = rowPair(1)
    (x, x2) <- row.zip(row2)
  } {
    x --> x2 tie = downTie
    x2 --> x tie = upTie
  }

  def color(col: Colorf): Unit = {
    for {
      row <- rowData
      n <- row
    } n.ui.color = col
  }
  color(Colorf.GHOSTWHITE)
}
