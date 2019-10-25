package memnets.ui

import memnets.model._

class Sliding(val layer: LayerLike, val divs: Int = 2048) extends ElementBase with GridData with Logging {
  name = "Sliding"
  // layer.loc.y at baseline, correct for plot.h
  loc = layer.ui.loc.up(layer.ui.plot.height / 2.0)
  val rows = 96
  var sampling = 5
  val cols: Int = if (layer.length <= divs) layer.length else divs
  val stride: Double = (layer.length - 1).toDouble / (cols - 1).toDouble
  var hints: GridHints = layer.ui.gridHints.getOrElse(GridHints())
  protected val empty: Array[Float] = Array.ofDim[Float](cols)
  protected val buffer: Array[Float] = Array.ofDim[Float](rows * cols)
  logger.debug(s"Sliding[layer= ${layer.name} x= $rows by y= $cols stride= $stride]")
  def yScale: Float = layer.y(0).ui.scale.getOrElseP(YRange.scaleF)
  override def reset(): Unit = {
    for (i <- 0 until rows)
      empty.copyToArray(buffer, i * empty.length)
  }
  override def preRender(te: Tick): Boolean = {
    val render = te.t % sampling == 0 || te.spikeCount > te.length / 4.0
    if (render) {
      buffer.copyToArray(buffer, cols) // shift over
      val layId = layer.id
      // sample
      var i = 0
      while (i < cols) {
        val off = (i * stride).asInstanceOf[Int]
        buffer(i) = te(layId, off).asInstanceOf[Float]
        i += 1
      }
    }
    render
  }
  def act(te: Tick, r: Int, c: Int): Double = { buffer(r * cols + c) }
}
