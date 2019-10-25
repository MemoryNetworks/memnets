package memnets.fx

import javafx.embed.swing.SwingFXUtils
import javafx.fxml.FXMLLoader
import javafx.scene.effect.BlendMode
import memnets.awt.PlotAwt
import memnets.fx.fx3d._
import memnets.model._
import memnets.ui._
import memnets.utils._
import scalafx.animation._
import scalafx.scene._
import scalafx.scene.canvas.Canvas
import scalafx.scene.image._
import scalafx.scene.layout.StackPane
import scalafx.scene.paint._
import scalafx.scene.shape._
import scalafx.scene.text._
import scalafx.util.Duration

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer

object BatterySkinFX extends Logging {
  def yscale(yb: Yb): Double = yb match {
    case y: Y if y.ui.scale.isDefined => 72.0 / y.ui.scale.get
    case default                      => 72.0 / YRange.scale
  }
}
class BatterySkinFX extends SkinFX with Logging {
  name = "Battery"
  import Math._

  import BatterySkinFX._
  import scalafx.Includes._
  import scalafx.scene.paint.Color
  @BeanProperty var oneDigit: Boolean = true

  override def init(system: DynamicSystem): Unit = {
    super.init(system)
    BatterySnapshot.init()
  }
  override def clearCanvas(canvas: Canvas) = {
    super.clearCanvas(canvas)
//    canvas.graphicsContext2D.lineCap = StrokeLineCap.Round
    canvas.delegate.getGraphicsContext2D.setLineCap(StrokeLineCap.Round.delegate)
  }
  def xscale(y: Yb): Double = xscale(y.ui.scale.getOrElse(YRange.scaleF))
  def xscale(yScale: Double) = if (yScale < YRange.scale) 0.8 else if (yScale > YRange.scale) 1.4 else 1.0
  def createY(y: Y) = new BatteryFX(y)
  override def createF(f: F) = {
    logger.debug("creating F = " + f.description)
    new FuncFX(f)
  }
  override def createYGoal(g: YGoal) = new GoalFX(g)
  override def createPlot(p: Plot) = new PlotFX(p)
  override def createImage(im: GridData) = new ImageFX(im, colorMapAwt)
  override def createPhasePlot(pl: PhasePlot) = {
    if (!pl.phases.isEmpty) {
      val scene = PhasePlot3DFX(pl, yColorMap)
      pl.onCreateScene(scene)
      create3d(scene)
    } else None
  }
  protected val DEFAULT_FONT_COL = Some(Color.WhiteSmoke.opacity(0.6))
  protected sealed class BatteryFX(ys: Y) extends TickableFX {
    import BatterySnapshot._
    val fx = new ImageView()
    fx.managed = false
    fx.pickOnBounds = true
    def node = fx
    var posEdgeCol = Color.White
    var negEdgeCol = Color.White
    var posCol: Color = _
    var negCol: Color = _
    var text: String = ""
    var yScale = YRange.scale
    val fontCol = if (ys.ui.showText) DEFAULT_FONT_COL else None
    val opacityProp = fx.delegate.opacityProperty
    final override def init(): Unit = {
      yScale = ys.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
      posCol = yColorMap(ys)
      negCol = posCol.interpolate(Color.Black, 0.3)
      posEdgeCol = posCol.brighter.brighter.opacity(0.6)
      negEdgeCol = negCol.darker.darker.opacity(0.6)
      fx.image = snapshot(ys)
      fx.delegate.setScaleX(rootScale)
      fx.delegate.setScaleY(rootScale)
      if (ys.ui.viz == Viz.Fade)
        opacityProp.set(0.0)
      val bnds = fx.delegate.getBoundsInParent
      fx.relocate(ys.ui.loc.x - bnds.getWidth, ys.ui.loc.y - bnds.getHeight - 31) // magic # (16)
      // do after layout
      fx.delegate.setScaleX(xscale(yScale) * rootScale)
    }
    override def reset() = {
      val omin = if (ys.ui.viz == Viz.Fade) 0.0 else 0.4
      opacityProp.set(omin)
      fx.delegate.setScaleX(xscale(yScale) * rootScale)
    }
    final def tick(te: Tick): Unit = {
      val gc = RenderContextFX.gc
      val act = ys.act
      var barCol = posCol
      var edgeCol = posEdgeCol
      val render =
        if (act < -0.01) {
          barCol = negCol
          edgeCol = negEdgeCol
          true
        } else
          act > 0.01

      val fade = (ys.id + te.t) % 3 == 0
      if (render) {
//        logger.debug("tick: "+te.t)
        val mag = abs(act)
        gc.globalAlpha = 1.0
        gc.fill = barCol
        var scale = xscale(yScale)
        val bscale = mag / yScale
        var barH = bscale * 72.0
        if (bscale > 1.0) {
          barH = 72.0
          scale = scale * bscale
        }
        fx.delegate.setScaleX(scale * rootScale)
        renderEdges(te, ys, mag, h, edgeCol, gc, alpha = 1.0)
        val barR = r * scale
        val x = ys.ui.loc.x
        val y = ys.ui.loc.y
        gc.fillRect(x - barR, y - barH, 2.0 * barR, barH)
        if (bestQuality) {
          // NOTE : COLOR_BURN needs bar drawn immediately before this, don't put edges in between....
          gc.globalBlendMode = BlendMode.COLOR_BURN
          gc.drawImage(frostImg, x - barR, y - barH, 2.0 * barR, barH)
          gc.globalBlendMode = BlendMode.SRC_OVER
        }
        if (fade) {
          val opacity = opacityProp.get + 0.05
          if (opacity < 1.0)
            opacityProp.set(1.2 * opacity) // system will cap at 1.0
        }
        if (fontCol.isDefined) {
          gc.font = font
          gc.fill = fontCol.get
          if ((te.t + ys.id) % 3 == 0) text = formatter.format(act)
          gc.fillText(text, x - text.length * 3.7, y - h - 4.0)
        }
      } else if (fade && ys.ui.viz < Viz.Focus) {
        val omin = if (ys.ui.viz == Viz.Fade) 0.0 else 0.4
        val opacity = opacityProp.get
        if (opacity > omin)
          opacityProp.set(opacity * 0.92)
      }
    }
    override def findTarget(x: Double, y: Double) =
      Some(new UserSource(ys, loc = ys.ui.loc, h = 2.0 * h - 9.0, scale = yscale(ys), xoff = 18.0, zoom = 2.0))
  }
  object FuncFX {
    val r = 20.0
    val h = BatterySnapshot.h
    val yRefl = 19.0
    val font = Font("Verdana", 10)
    val ui: JGroup = "func.fxml".loadFXML
    val root = new Group(ui)
    val text = root.children(1).asInstanceOf[JBorderPane].getCenter.asInstanceOf[JText]
    val tempScene = new Scene(400, 400) // magic # ???
    tempScene.content = root
    val params = new SnapshotParameters()
    params.fill = Color.Transparent
    // hack : w/o "warm-up" 1st guy has bad bounds
    root.snapshot(params, new WritableImage(100, 100))
    def snapshot(f: F): Image = {
      text.setText(f.description)
      val w = root.boundsInLocal.value.getWidth.asInstanceOf[Int]
      val h = root.boundsInLocal.value.getHeight.asInstanceOf[Int]
      root.snapshot(params, new WritableImage(w, h))
    }
  }
  protected sealed class FuncFX(f: F) extends TickableFX {
    import FuncFX._
    val fx = new ImageView()
    fx.managed = false
    fx.pickOnBounds = true
    def node = fx
    val posCol = FUNC_COLOR
    val highCol = posCol.darker
    val yCache = ArrayBuffer[(Y, Col)]()
    var yScale = YRange.scaleF
    val outScale = edgeScale
    override def init(): Unit = {
      val loc = f.owner.ui.loc
      val y = loc.y
      val x = loc.x
      val yF = y + 50.0
      yScale = f.scale.getOrElse(YRange.scaleF)
      yCache.clear
      for (in <- f.inputs) {
        val tuple = (in, yColorMap(in))
        yCache += tuple
      }
      fx.image = snapshot(f)
      if (f.ui.viz == Viz.Fade)
        opacityProp.set(0.0)
      fx.relocate(x - fx.getBoundsInLocal.getWidth / 2.0, yF - fx.getBoundsInLocal.getHeight + 20.0)
    }
    override def reset() = {
      val omin = if (f.ui.viz == Viz.Fade) 0.0 else 0.4
      opacityProp.set(omin)
    }
    private final def render(te: Tick, vizLevel: Viz, a: Double, pCol: Paint, fade: Boolean): Unit = {
      val gc = RenderContextFX.gc
      val loc = f.owner.ui.loc
      val x = loc.x
      val y = loc.y
      val yF = y + 62.0
      gc.globalAlpha = 0.4
      gc.stroke = pCol
      val lw = outScale * a
      gc.lineWidth = if (lw > 20.0) 20.0 else lw
      gc.strokeLine(x, yF - 14.0, x, y + 12.0)
      var i = 0
      val len = yCache.length
      while (i < len) {
        val fd = yCache(i)
        val tgt = fd._1
        val ta = Math.abs(tgt.act)
        if (ta > 0.01f && tgt.ui.viz >= vizLevel) {
          gc.stroke = fd._2
          val lw = outScale * ta
          gc.lineWidth = if (lw > 12.0) 12.0 else lw
          gc.strokeLine(tgt.ui.loc.x, tgt.ui.loc.y - h + 3.0, x, yF)
        }
        i += 1
      }
      var scale = 1.0
      if (a > yScale) scale = a / yScale
      if (scale > 1.3) scale = 1.3
      fx.delegate.scaleXProperty.set(scale)
      if (fade) {
        val opacity = opacityProp.get + 0.01
        if (opacity < 1.0)
          opacityProp.set(1.7 * opacity)
      }
    }
    val opacityProp = fx.delegate.opacityProperty
    def tick(te: Tick): Unit = {
      val fade = te.t % 3 == 0
      if (f.act > 0.001)
        render(te, vizLevel, f.act, posCol, fade)
      else if (f.act < -0.001)
        render(te, vizLevel, -f.act, highCol, fade)
      else if (fade) {
        val opacity = opacityProp.get
        if (opacity > 0.0)
          opacityProp.set(opacity * 0.7)
      }
    }
  }
  protected[memnets] class PlotFX(val plot: Plot) extends TickableFX {
    import VariableType._
    val font = Font("Tahoma", 13)
    val ui: JStackPane = "plot.fxml".loadFXML
    val fx = new StackPane(ui)
    def node = fx
    fx.managed = false
    fx.userData = plot
    val plotfx = new ImageView(fx.children(0).asInstanceOf[JImageView])
    val glass = fx.children(1).asInstanceOf[JRectangle]
    def hideGlass: Unit = {
      glass.getStyleClass.removeAll("glass")
      glass.fill = null
    }
    if (plot.hideGlass) hideGlass
    if (plot.useEffect)
      hideGlass
    else
      plotfx.effect = null

    val w = plot.width
    val h = plot.height
    val image = new WritableImage(w.toInt, h.toInt)
    plotfx.image = image
    plotfx.maxWidth(w)
    plotfx.fitHeight = h
    plotfx.fitWidth = w
    glass.heightProperty.setValue(h)
    glass.widthProperty.setValue(w)
    val plotAwt = new PlotAwt(plot, w.toInt, h.toInt, yColorMap, edgeScale)
    override def init(): Unit = {
      fx.layout()
      fx.relocate(plot.loc.x, plot.loc.y - h / 2.0)
      plotAwt.init()
    }
    def tick(te: Tick): Unit = {
      plotAwt.tick()
      SwingFXUtils.toFXImage(plotAwt.bufferedImage, image)
      val gc = RenderContextFX.gc
      val foundSoftMax =
        renderLayerLinks(RenderContextFX.gc, plotAwt, h, 8.0, BatterySnapshot.h, dotWeightsVisible, font)
      if (foundSoftMax > 0.0) {
        val perc = foundSoftMax
        gc.globalAlpha = 0.8
        gc.font = font
        var msg = "confident"
        if (perc >= 0.5)
          gc.fill = WIN_COLOR
        else if (perc > 0.15) {
          gc.fill = Color.White
          msg = "not sure"
        } else {
          gc.fill = LOSE_COLOR
          msg = "not confident"
        }
        val floc = Loc(plot.loc.x - w / 2 + 2, plot.loc.y - h + 22.0)
        gc.fillText(msg, floc.x, floc.y)
      }
    }
    override def findTarget(x0: Double, y0: Double) = {
      val lay = plot.layers.head // NOTE : only picks top layer
      val wD = w
      var id = ((x0 + wD / 2.0) / wD * lay.length.toDouble).toInt
      id = if (id < 0) 0 else if (id > lay.length) lay.length else id
      logger.debug(s"Layer[name: ${lay.name}] pick id = $id")
      val y = lay.y(id)
      val loc = if (lay.ui.numericalType == Continuous) y.ui.loc.left(lay.ui.spacing(w) / 2.0) else y.ui.loc
//      val range = lay.ui.plot.range
//      val scale = h / range.range
//      val us = if (range.isMinZero)
//        UserSource(y, loc, h = h / 2.0, scale = scale)
//      else
//        UserSource(y, loc.up(h / 2.0), scale = scale)

      val us = UserSource(y, loc.up(plotAwt.zero), scale = plotAwt.hScale, h = h / 2 - plotAwt.zero)
      Some(us)
    }
  }
  protected sealed class GoalFX(val goal: YGoal) extends Group with TickableFX {
    mouseTransparent = true
    override def permanent = false
    def node = this
    val rect = Rectangle(width = 2.0 * BatterySnapshot.r * xscale(goal.tgt), height = 4.0)
    val text = new Text("")
    text.visible = false
    text.font = Font("Tahoma", 20)
    text.layoutX = 2
    text.layoutY = -10
    children.addAll(rect, text)
    if (goal.isGood.get) {
      rect.fill = WIN_COLOR
      text.fill = WIN_COLOR
      text.text = f"+${goal.reward}%.0f"
    } else {
      rect.fill = LOSE_COLOR
      text.fill = LOSE_COLOR
      text.text = f"${goal.reward}%.0f"
    }
    opacity = 0.25
    val yOff = goal.expected * yscale(goal.tgt)
    val loc = goal.tgt.ui.loc
    relocate(loc.x - rect.width.value / 2.0, loc.y - rect.height.value - yOff + 1.0)
    def tick(te: Tick): Unit = {
      if (!intro.playing && te.even && goal.tgt.act > 0.0) {
        delegate.setOpacity(0.4 + 0.5 * cos(2.0 * Math.PI * (0.005 * goal.tgt.act / goal.expected + 0.01) * te.t))
      }
    }
    override def reset(): Unit = {
      super.reset()
      //     opacity.unbind()
    }
    override def addAnim = intro
    override def delAnim = if (goal.isCompleted) outro else None
    override def toString = s"Goal for tgt= ${goal.tgt}"
    protected val intro = new SequentialTransition(node = this) {
      onFinished = _ => {
        node.value.toBack()
//        GoalFX.this.opacity <== -(goal.timeProgress*0.9) + 1.0
      } // make mouse transparent
      rate = 1.2 // a bit faster
      children = Seq(
        new TranslateTransition {
          duration = Duration(300)
          toY = if (goal.isGood.get) -25.0 else 10.0
          interpolator = Interpolator.EaseBoth
        },
        new ParallelTransition {
          cycleCount = 2
          children = Seq(
            new ScaleTransition {
              duration = Duration(350)
              fromX = 1.0
              toX = 2.5
              fromY = 1.0
              toY = 2.5
              cycleCount = 2
              autoReverse = true
              interpolator = Interpolator.EaseBoth
            },
            new FadeTransition {
              duration = Duration(150)
              fromValue = 1.0
              toValue = 0.0
              cycleCount = 4
              autoReverse = true
              interpolator = Interpolator.EaseBoth
            }
          )
        },
        new TranslateTransition {
          duration = Duration(300)
          toY = 0
          interpolator = Interpolator.EaseBoth
        }
      )
    }
    protected lazy val outro = new SequentialTransition(node = this) {
      cycleCount = 1
      children = Seq(
        new PauseTransition {
          duration = Duration(16)
          onFinished = _ => {
            opacity = 0.8
            text.visible = true
          }
        },
        new ScaleTransition {
          duration = Duration(800)
          fromX = 1.0
          toX = 6.0
          fromY = 1.0
          toY = 6.0
          interpolator = Interpolator.EaseBoth
        },
        new ScaleTransition {
          duration = Duration(400)
          fromX = 6.0
          toX = 0.0
          fromY = 6.0
          toY = 0.0
          interpolator = Interpolator.EaseBoth
        }
      )
    }
  }
  object BatterySnapshot {
    import Color._
    val oneDigit: Boolean = true
    val BLACK50 = Black.opacity(0.5)
    val BLACK80 = Black.opacity(0.8)
    val WHITE30 = White.opacity(0.3)
    val r = 18.0
    val h = 87.0
    val uRad = 32.0
    val yRefl = 19.0
    val font = Font(family = "Tahoma", size = 15)
    val root = new Group(FXMLLoader.load("battery.fxml".asURL))
    val text = root.findTextById("text")
    val highlight = new Rectangle(root.findById("highlight"))
    val formatter: FastFormatter = if (oneDigit) OneDigit else TwoDigits
    // need scene for css + layout to work
    val tempScene = new Scene(
      width = 200,
      height = 200,
      depthBuffer = true,
      antiAliasing = SceneAntialiasing.Balanced
    ) // magic # ???
    tempScene.content = root
    val params = new SnapshotParameters()
    params.fill = Color.Transparent
    val endStop = Stop(1.0, Color.Transparent)
    // scale up gives high quality render
    root.scaleX = 2.0
    root.scaleY = 2.0
    val rootScale = 0.5
    val fill = new Rectangle(root.findById("fill"))
    fill.visible = false
    val frost = new Rectangle(root.findById("frost"))
    val frostImg = frost.snapshot(params, new WritableImage(36, 72))
    frost.visible = false
    val base = new Rectangle(root.findById("base"))
    val cap = new Rectangle(root.findById("cap"))
    protected var isInit = false
    def init(): Unit = {
      if (!isInit) {
        // hack : w/o "warm-up" 1st guy has bad bounds
        root.snapshot(params, new WritableImage(46, 101))
        isInit = true
      }
    }
    def snapshot(y: Y): Image = {
      val txt = y.description
      text.setText(txt)
      if (txt.length > 2) {
        text.setRotate(0.0)
        text.setTranslateY(0.0)
      } else {
        text.setRotate(90.0)
        text.setTranslateY(1.5)
      }
      highlight.visible = y.ui.viz == Viz.User
      root.layout()
      val bnds = root.boundsInParent.value
      val w = bnds.getWidth.asInstanceOf[Int]
      val h = bnds.getHeight.asInstanceOf[Int]
      root.snapshot(params, new WritableImage(w, h))
    }
  }
}
