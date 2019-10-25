package memnets.fx

import memnets.model._
import scalafx.scene.paint._

import scala.collection.mutable._

object SignalFX {
  def apply(
      signal: Signal,
      yColorMap: YCol,
      window: Int = 25,
      sampling: Int = 3,
      yScale: Option[Double] = None,
      showTracer: Boolean = true
  ): SignalFX = {
    val fx = new SignalFX(signal, window, yColorMap.trackColor(signal.tgt))
    fx.showTracer = showTracer
    fx.sampling = sampling
    fx.yScale = yScale.getOrElseP(BatterySkinFX.yscale(signal.tgt))
    fx
  }
}
sealed class SignalFX(val signal: Signal, override val window: Int, override val tgtCol: Color)
    extends TracerFX(signal.tgt, window, tgtCol) {
  override val element = signal
  override def permanent = false
  override def offsets: (Loc, Double, Double) = signal match {
    case rt: Signal if rt.src != null =>
      (rt.src.loc, rt.src.xoff, rt.src.scale)
    case s: Signal =>
      (s.tgt.ui.loc, if (tgt.isInstanceOf[Y]) 18.0 else 2.0, yScale)
  }
  protected override def shift: Unit = {
    // 0+1 locked at y, 2 is oldest
    super.shift
    buffer.remove(2)
  }
  protected override def sample(): Unit = {
    super.sample()
    buffer += y - scale * signal.act
  }

  val buffer = ArrayBuffer[Double]()
  val bufferArray = Array.ofDim[Double](len)
  var sCol: JLinearGradient = _
  override def init(): Unit = {
    super.init()
    buffer.sizeHint(len)
    buffer += y
    buffer += y
    sCol = LinearGradientP(
      startX = x,
      startY = y - 1,
      endX = x,
      endY = y + 1,
      proportional = false,
      stops = Seq(Stop(0, tgtCol.opac(0.7)), Stop(1, Color.rgb(0, 0, 0, 0.7))))
  }
  override def tick(te: Tick): Unit = {
    super.tick(te)
    val gc = RenderContextFX.gc
    // signal shape
    gc.globalAlpha = 0.5
    gc.setFill(sCol)
    val size = buffer.length
    buffer.copyToArray(bufferArray, 0, size)
    gc.fillPolygon(xArray, bufferArray, size)
  }
  override def toString = s"SignalFX[sig = $signal]"
}
