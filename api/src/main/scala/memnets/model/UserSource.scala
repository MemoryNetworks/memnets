package memnets.model

case class UserSource(
    tgt: Yb,
    loc: Loc,
    scale: Double = YRange.scale,
    h: Double = 0.0,
    xoff: Double = 0.0,
    zoom: Double = 1.0
) {
  def this(y: Yb, h: Double, xoff: Double) {
    this(y, y.ui.loc, h = h, xoff = xoff)
  }
  // y is down in fx.
  def calcAct(y: Double) : Double = -(y - h) / (scale * zoom)
}
