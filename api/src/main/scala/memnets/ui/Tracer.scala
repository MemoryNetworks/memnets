package memnets.ui

import memnets.model._

case class Tracer(
    tgt: Yb,
    window: Int = 25,
    sampling: Int = 3,
    yScale: Option[Double] = None
)(implicit model: DynamicSystem)
    extends Element {
  var name = s"Tracer for ${tgt.name}"
  def ui: YbUI = tgt.ui
  model.elements += this
}
