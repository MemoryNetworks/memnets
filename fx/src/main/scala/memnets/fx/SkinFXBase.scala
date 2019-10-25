package memnets.fx

import java.lang.Math.abs

import javafx.scene.effect.BlendMode
import memnets.awt.PlotAwt
import memnets.model._
import memnets.ui._
import scalafx.scene.Node
import scalafx.scene.paint._
import scalafx.scene.text.Font

import scala.beans._
import scala.collection.mutable

abstract class SkinFXBase extends Skin[Node, Col] {

  /**
   * NOTE : even with canvas locked at 720p, blur effect will cut FPS down to 30fps
   * on embedded HD graphics if app resolution goes beyond 720
   */
  @BeanProperty var backBlend = BlendMode.SRC_OVER
  @BeanProperty var canvasBlend = BlendMode.SRC_OVER
  @BeanProperty var canvasEffect = EffectType.None
  @BeanProperty var style = ""
  @BeanProperty var fullScreenStyle = ""
  @BooleanBeanProperty var customClock = false
  @BooleanBeanProperty var customTimeChart = false
  @BooleanBeanProperty var dotWeightsVisible = true

  protected var _colorMapToAwts = mutable.AnyRefMap[ColMap, ColorMap[java.awt.Color]]()
  protected object _gradientColorMap extends YGradientFX with CustomColorsFX

  def colorMapAwt: ColorMap[java.awt.Color] = {
    val cm = colorMap
    _colorMapToAwts.getOrElseUpdate(cm, {
      cm.toAwt()
    })
  }
  override def imageLookup(i: SkinImage): Option[String] = {
    import SkinImage._
    val back: String = i match {
      case ZERO    => "tiles-backgroundv0.jpg"
      case ONE     => "tiles-backgroundv1.jpg"
      case TWO     => "tiles-backgroundv2.jpg"
      case THREE   => "tiles-backgroundv3.jpg"
      case FOUR    => "tiles-backgroundv4.jpg"
      case FIVE    => "tiles-backgroundv5.jpg"
      case SIX     => "tiles-backgroundv6.jpg"
      case default => null
    }
    if (back == null) None else Some("/backgrounds/" + back)
  }
  def timeChartLookup(i: SkinImage): Option[String] = None
  def lineWidthMax = 6.0

  protected def heatMap: ColMap = HEAT_COLORMAP
  protected def opacityMap: ColMap = OPACITY_COLORMAP
  protected def grayMap: ColMap = GRAY_COLORMAP
  protected def invertedGrayMap: ColMap = INVERTED_GRAY_COLORMAP
  protected def convertToColHelper(colorf: Colorf): Col = Color(colorf.r, colorf.g, colorf.b, colorf.a)
  protected def createYGradientMap(model: DynamicSystem): YGradientMap[Col] = {
    if (gradientCustom)
      _gradientColorMap
    else
      YGradientFX()
  }
  protected def renderEdges(
      te: Tick,
      src: Y,
      mag: Double,
      h: Double,
      edgeCol: Color,
      gc: GC = RenderContextFX.gc,
      alpha: Double = 0.6
  ): Unit = {

    val wScale = edgeScale
    if (wScale > 0.0) {
      val lwMax = lineWidthMax
      val viz = vizLevel
      val x = src.ui.loc.x
      val y = src.ui.loc.y
      gc.globalAlpha = alpha
      for (e <- src.outsNoLoops) {
        if (te.isActive(e.tgt)) {
          val tgt = te.system.variables(e.tgt)
          if (tgt.ui.viz >= viz) {
            val w = e.w
            val absmag = if (w > 0.0) {
              gc.stroke = edgeCol
              mag
            } else {
              gc.stroke = INH_EDGE_COLOR
              -mag
            }
            val lw = wScale * absmag * w
            gc.lineWidth = if (lw > lwMax) lwMax else lw
            val tx = tgt.ui.loc.x
            val ty = tgt.ui.loc.y
            if (ty > y && abs(tx - x) < 2.0) {
              val xMid = x - 20.0
              val yStart = y - h + 5.0
              val yEnd = ty + 12.0
              val yMid = yStart + (yEnd - yStart) / 2.0
              gc.strokeLine(x, yStart, xMid, yMid)
              gc.strokeLine(xMid, yMid + 2.0, tx, yEnd)
            } else
              gc.strokeLine(x, y - h + 3.0, tx, ty + 12.0)
          }
        }
        gc.globalAlpha = 1.0
      }
    }
  }

  /** NOTE: unlike yScale, outScale multiplies not divides */
  protected def renderLayerLink(
      gc: GC,
      loc: Loc,
      h: Double,
      tLoc: Loc,
      th: Double,
      act: Double,
      w: Double,
      col: Paint,
      wScale: Double
  ) = {
    gc.stroke = col
    val lw = wScale * Math.abs(w * act)
    gc.lineWidth = if (lw > 12.0) 12.0 else lw
    gc.strokeLine(loc.x, loc.y - h, tLoc.x, tLoc.y + th)
  }

  /** @return if special case of softmax (hackish), then topK(0).act else -1.0 */
  protected def renderLayerLinks(
      gc: GC,
      plotAwt: PlotAwt,
      h: Double,
      th: Double,
      yh: Double,
      showDotWeights: Boolean,
      font: Font
  ): Double = {
    var softMax = -1.0
    val renders = plotAwt.renders
    var i = 0
    while (i < renders.length) {
      val info = renders(i)
      import info._
      if (layer.lastTopK.isDefined) {
        val top = layer.lastTopK.get
        val formatter = layer.ui.format
        val len = top.entries.length
        var j = 0
        while (j < len) {
          val entry = top.entries(j)
          val y = entry.y
          val eCol = yColorMap(entry.y)
          val loc = y.ui.loc
          val act = y.act
          for (e <- layer.outLinks) {
            gc.globalAlpha = 0.4
            e match {
              case divt: DivergeTied if divt.src.ui.isShown && divt.tgtRange.contains(entry.index) =>
                val sAct = divt.src.act
                if (sAct > 0.01)
                  renderLayerLink(gc, divt.src.ui.loc, yh, loc, th, sAct, divt.w, yColorMap(divt.src), wScale)
              case dott: DotTied if dott.tgt.ui.isShown && dott.srcRange.contains(entry.index) =>
                val tLoc = dott.tgt.ui.loc
                renderLayerLink(gc, loc, h, tLoc, th, act, dott.w, eCol, wScale)
              case mat: MatMul if mat.tgt.ui.isShown =>
                for {
                  tgtTopK <- mat.tgt.lastTopK
                  tgtTop <- tgtTopK.entries
                } renderLayerLink(gc, loc, h, tgtTop.y.ui.loc, th, act, mat(tgtTop.index, entry.index), eCol, wScale)
              case dot: Dot if dot.tgt.ui.isShown =>
                val tLoc = dot.tgt.ui.loc
                val w = dot(entry.index)
                val eCol2 = if (w > 0.0) eCol else eCol.interpolate(Color.Black, 0.5)
                renderLayerLink(gc, loc, h, tLoc, th, act, w, eCol2, wScale)
                if (showDotWeights) {
                  gc.globalAlpha = 0.7
                  val w2 = 6.0
                  val h2 = abs(w * 40.0)
                  gc.fill = if (w < 0.0) Color.Crimson else Color.LimeGreen
                  gc.fillRect(loc.x - 2.0 * w2, loc.y - h - h2, w2, h2)
                }
              case div: Diverge if div.src.ui.isShown =>
                val sAct = div.src.act
                if (sAct > 0.01)
                  renderLayerLink(gc, div.src.ui.loc, yh, loc, th, sAct, div.w(entry.index), yColorMap(div.src), wScale)
              case den: DenseLink if den.tgt.ui.isShown =>
                val tgt = den.tgt.y(entry.index)
                renderLayerLink(gc, loc, h, tgt.ui.loc, th, act, den(entry.index), eCol, wScale)
              case spr: SparseLink if spr.tgt.ui.isShown =>
                for {
                  tgtTopK <- spr.tgt.lastTopK
                  tgtTop <- tgtTopK.entries
                } renderLayerLink(gc, loc, h, tgtTop.y.ui.loc, th, act, spr(tgtTop.index, entry.index), eCol, wScale)
              case default =>
            }
          }
          // do last so on top others
          if (showText) {
            gc.globalAlpha = 1.0
            gc.font = font
            gc.fill = Color.Gray
            gc.save
            gc.translate(loc.x, loc.y - h - 6.0)
            gc.rotate(-45.0)
            gc.fillText(formatter.format(entry.index, entry.y.act), 0.0, 0.0)
            gc.restore
          }
          j += 1
        }
        if (showText && isSoftMax && top.entries.length > 0)
          softMax = top.entries(0).act

      }
      i += 1
    }
    softMax
  }
}
