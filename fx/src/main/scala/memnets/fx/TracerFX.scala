package memnets.fx

import memnets.model._
import memnets.ui.Tracer
import scalafx.Includes._
import scalafx.scene.paint._

import scala.collection.mutable._

object TracerFX {
  def apply(tracer: Tracer, tgtColor: Color): TracerFX = {
    val fx = new TracerFX(tracer.tgt, tracer.window, tgtColor)
    fx.sampling = tracer.sampling
    fx.yScale = tracer.yScale.getOrElseP(BatterySkinFX.yscale(tracer.tgt))
    fx
  }
}

class TracerFX(val tgt: Yb, val window: Int, val tgtCol: Color) extends TickableFX {
  protected val len = window + 2
  protected val xArray = Array.ofDim[Double](len)
  private val nbuffer = ArrayBuffer[Double]()
  private val nbufferArray = Array.ofDim[Double](len)
  protected var showTracer = true
  protected var x, y = 0.0
  protected var scale = 0.0
  private var nCol: JLinearGradient = _
  var sampling = 3
  var yScale = 1.0 // NOTE: this is in pixels
  override def element: Element = null
  override def init(): Unit = {
    super.init()
    // loc can change during layout
    val tuple = offsets
    val loc = tuple._1
    val xOff = tuple._2
    scale = tuple._3
    x = loc.x
    y = loc.y
    nbuffer.sizeHint(len)
    nbuffer += y
    nbuffer += y
    // start at node, go left for bottom, then up and across with samples
    xArray(0) = loc.x - xOff
    nCol = LinearGradientP(
      startX = x,
      startY = y - 1,
      endX = x,
      endY = y + 1,
      proportional = false,
      stops = Seq(Stop(0, tgtCol.opacity(0.8)), Stop(1, tgtCol.darker.darker.opacity(0.8))))

  }
  override def node = None
  override def toString = s"TracerFX[tgt= ${tgt}]"
  def tick(te: Tick): Unit = {
    val gc = RenderContextFX.gc
    if (te.t % sampling == 0 || tgt.isSpiking) {
      sample()
      if (nbuffer.length > xArray.length)
        shift
    }
    // update x values
    // start at node (0), go left for bottom (1), then directly up (2), and across with samples
    if (nbuffer.length <= xArray.length) {
      val n = nbuffer.length - 2
      val xStart = xArray(0) - 4.0 * (n - 1)
      xArray(1) = xStart
      var i = 0
      while (i < n) {
        xArray(i + 2) = xStart + 4.0 * i
        i += 1
      }
    }
    if (showTracer) {
      // response shape
      val size = nbuffer.length
      gc.globalAlpha = 0.4
      gc.setFill(nCol) // avoid paint conversion
      nbuffer.copyToArray(nbufferArray, 0, size)
      gc.fillPolygon(xArray, nbufferArray, size)
    }
  }
  protected def offsets: (Loc, Double, Double) = (tgt.ui.loc, 16.0, yScale)
  protected def shift(): Unit = {
    // 0+1 locked at y, 2 is oldest
    nbuffer.remove(2)
  }
  protected def sample(): Unit = {
    // response
    nbuffer += y - (scale * tgt.act)
  }
}
