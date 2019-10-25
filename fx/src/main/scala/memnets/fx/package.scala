package memnets

import java.io.File
import java.util.concurrent._

import javafx.fxml.FXMLLoader
import javafx.scene.control.DialogPane
import javafx.scene.{paint => jfxp}
import memnets.core.MediaSound
import memnets.model._
import memnets.ui.{ColorMap, YColorMap}
import memnets.utils._
import org.fxyz3d.shapes.containers.ShapeContainer
import scalafx.Includes._
import scalafx.animation.Timeline
import scalafx.application.Platform
import scalafx.beans.property._
import scalafx.event.subscriptions.Subscription
import scalafx.scene._
import scalafx.scene.input._
import scalafx.scene.media._
import scalafx.scene.paint.Color
import scalafx.scene.shape.MeshView
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage._

import scala.collection.mutable.ArrayBuffer

package object fx extends Logging {
  type JColor = jfxp.Color
  type Col = scalafx.scene.paint.Color
  type YCol = YColorMap[Col]
  type ColMap = ColorMap[Col]
  type ColAwt = java.awt.Color
  val HEAT_COLORMAP: ColMap = HeatMapFX()
  lazy val OPACITY_COLORMAP: ColMap = OpacityMapFX()
  lazy val GRAY_COLORMAP: ColMap = GrayMapFX()
  lazy val INVERTED_GRAY_COLORMAP: ColMap = InvertedGrayMapFX()
  implicit def colorMap2Option(pal: ColMap): Option[ColMap] = Option(pal)

  implicit def ycolMapFXToYColMapAwt(colMap: YCol): YColorMap[java.awt.Color] = new YColorMapAwtImpl(colMap)
  final class YColorMapAwtImpl private[memnets] (colMap: YCol) extends YColorMap[ColAwt] {
    @inline override def apply(y: Y): ColAwt = colMap(y).toAwt()
    @inline override def apply(yb: Yb): ColAwt = colMap(yb).toAwt()
    @inline override def apply(layer: LayerLike, i: Int): ColAwt = colMap(layer, i).toAwt()
    @inline override def apply(layer: LayerLike): ColAwt = colMap(layer).toAwt()
    @inline def apply(i: Int, length: Int): ColAwt = colMap(i, length).toAwt()
  }
  // scalafx sugar
  implicit class BooleanPropertyExt(val p: ReadOnlyBooleanProperty) extends AnyVal {
    def ==>(chg: Boolean => Unit): Subscription = {
      p.onChange { (_, _, b) =>
        chg(b)
      }
    }
  }
  implicit class DoublePropertyExt(val p: ReadOnlyDoubleProperty) extends AnyVal {
    def ==>(chg: Double => Unit): Subscription = {
      p.onChange { (_, _, b) =>
        chg(b.doubleValue)
      }
    }
  }
  implicit class IntegerPropertyExt(val p: ReadOnlyIntegerProperty) extends AnyVal {
    def ==>(chg: Int => Unit): Subscription = {
      p.onChange { (_, _, b) =>
        chg(b.intValue)
      }
    }
  }
  implicit class IntegerPropertyExt2(val p: IntegerProperty) extends AnyVal {
    def +=(v: Int): Unit = {
      p.value = p.value + v
    }
  }
  implicit class StringPropertyExt(val p: ReadOnlyStringProperty) extends AnyVal {
    def ==>(chg: String => Unit): Subscription = {
      p.onChange { (_, _, b) =>
        chg(b)
      }
    }
  }
  implicit class ObjectPropertyExt[T, O <% ReadOnlyObjectProperty[T]](val p: O) {
    def ==>(chg: T => Unit): Subscription = {
      p.onChange { (_, _, b) =>
        chg(b)
      }
    }
  }

  implicit class DialogPaneExt(val dp: DialogPane) {
    def darkButtons(): Unit = {
      // hack but tired of trying to find right nested css class...
      val buttonStyle = """-fx-text-fill: #888;
                          |-fx-base: #333;
                          |// memnet-color
                          |-fx-focus-color: #8C5FD2;
                          |-fx-shadow-highlight-color: #222;
                          |""".stripMargin

      for {
        bt <- dp.getButtonTypes
        b = dp.lookupButton(bt)
      } b.style = buttonStyle

    }
  }

  val INH_COLOR = Color.web("#bbb")
  val INH_EDGE_COLOR = INH_COLOR.opac(0.6)
  val FUNC_COLOR = Color.web("#bbb")
  val ACC_COLOR = Color.LimeGreen
  val USER_COLOR = Color.WhiteSmoke.interpolate(Color.LimeGreen, 0.4).sat(0.75)
  val defaultCol = Color.LimeGreen

  implicit class FXYUIExt(val yui: YUI) extends AnyVal {
    def colorfx: Col = {
      val colfOpt = yui.color
      if (colfOpt.isDefined)
        colfOpt.get.converted.asInstanceOf[Col]
      else
        null
    }
  }
  implicit def colToColf(c: Col): Colorf = {
    val colf = Colorf(c.getRed, c.getGreen, c.getBlue, c.getOpacity)
    colf.converted = c
    colf
  }
  implicit def colfToCol(c: Colorf): JColor = Color(c.r, c.g, c.b, c.a)
  implicit class FxLayerUIExt(val lui: LayerUI) extends AnyVal {
    def colorfx: Option[YCol] = lui.owner.get[YCol](ui.YCOLMAP)
    def colorfx_=(ycolMap: YCol): Unit = { lui.owner.update(ui.YCOLMAP, ycolMap) }
  }
  implicit def col2GradHints(c: Col): GradientHints = {
    GradientHints(
      hue = (360.0 * c.hue).asInstanceOf[Float],
      spectrum = 360,
      saturation = c.saturation.asInstanceOf[Float],
      brightness = c.brightness.asInstanceOf[Float],
      minDivs = 100000,
      opacity = c.opacity.asInstanceOf[Float]
    )
  }
  implicit class YIterExt[T <: Iterable[Y]](val iter: T) extends AnyVal {
    def color(color: Color, shift: Double = 15.0, sat: Double = 0.8): Unit = {
      val colorAdj = color.sat(sat)
      for ((v, i) <- iter.zipWithIndex) v.ui.color = colorAdj.hueShift(i * shift)
    }
  }
  implicit def colToOpt(c: Col) = Option(c)
  implicit def jColToOpt(c: JColor) = Option(c)
  implicit class ColorFxExt(val c: Color) extends AnyVal {
    import scalafx.Includes.jfxColor2sfx
    // NOTE : returns Color, not JColor like opacity
    def trans(alpha: Double = 0.2): Color = Color(c.getRed, c.getGreen, c.getBlue, alpha)
    def opac(o: scala.Double): Color = c.opacity(o)
    def sat(f: scala.Double): Color =
      c.deriveColor(hueShift = 0.0, saturationFactor = f, brightnessFactor = 1.0, opacityFactor = 1.0)
    def hueShift(shift: scala.Double): Color =
      c.deriveColor(hueShift = shift, saturationFactor = 1.0, brightnessFactor = 1.0, opacityFactor = 1.0)
    def toRGB = c.delegate.toRGB
    def toRGBA = c.delegate.toRGBA
    def toAwt(useAlpha: Boolean = true) = c.delegate.toAwt(useAlpha)
  }
  implicit class JColorFxExt(val c: JColor) extends AnyVal {
    def toRGB = f"#${toInt(c.getRed)}%02X${toInt(c.getGreen)}%02X${toInt(c.getBlue)}%02X".toLowerCase
    def toRGBA =
      f"#${toInt(c.getRed)}%02X${toInt(c.getGreen)}%02X${toInt(c.getBlue)}%02X${toInt(c.getOpacity)}%02X".toLowerCase
    def toAwt(useAlpha: Boolean = true): java.awt.Color = {
      implicit def d2f(d: Double): Float = d.asInstanceOf[Float]
      new java.awt.Color(
        c.getRed,
        c.getGreen,
        c.getBlue,
        if (useAlpha) c.getOpacity else 1.0
      )
    }
    private def toInt(d: Double): Int = (d * 255).toInt
  }
  val WIN_COLOR = Color.LimeGreen.sat(0.8)
  val LOSE_COLOR = Color.Crimson.sat(0.8)

  type JTask = javafx.concurrent.Task[Unit]
  type JCycleMethod = jfxp.CycleMethod
  type JLinearGradient = jfxp.LinearGradient
  type JStop = jfxp.Stop
  type JGroup = javafx.scene.Group
  type JNode = javafx.scene.Node
  type JShape3D = javafx.scene.shape.Shape3D
  type JRectangle = javafx.scene.shape.Rectangle
  type JText = javafx.scene.text.Text
  type JTextArea = javafx.scene.control.TextArea
  type JTextFlow = javafx.scene.text.TextFlow
  type JBorderPane = javafx.scene.layout.BorderPane
  type JStackPane = javafx.scene.layout.StackPane
  type JVBox = javafx.scene.layout.VBox
  type JAnchorPane = javafx.scene.layout.AnchorPane
  type JPane = javafx.scene.layout.Pane
  type JScrollPane = javafx.scene.control.ScrollPane
  type JImageView = javafx.scene.image.ImageView
  type JToggleButton = javafx.scene.control.ToggleButton
  type JButton = javafx.scene.control.Button
  type JColorPicker = javafx.scene.control.ColorPicker
  type JLabel = javafx.scene.control.Label
  type JMouseEvent = javafx.scene.input.MouseEvent
  type JPhong = javafx.scene.paint.PhongMaterial
  type JMaterial = javafx.scene.paint.Material
  type JPaint = javafx.scene.paint.Paint
  type JTriangleMesh = javafx.scene.shape.TriangleMesh

  type JMeshView = javafx.scene.shape.MeshView
  type GC = scalafx.scene.canvas.GraphicsContext

  implicit class ColorMapFXExt(val colMap: ColMap) extends AnyVal {
    def toImage(): javafx.scene.image.Image = {
      import colMap._
      import javafx.scene.image._
      // try to create a square image
      val imgPalette = new WritableImage(width, height)
      val pw = imgPalette.getPixelWriter
      var index = 0
      val w = width
      val h = height
      var y = 0
      while (y < h) {
        var x = 0
        while (x < w) {
          pw.setColor(x, y, colors(index))
          index += 1
          x += 1
        }
        y += 1
      }
      imgPalette
    }
    def toPhong(): JPhong = {
      val mat = new JPhong
      mat.setDiffuseMap(toImage())
      mat.setSpecularColor(Color.web("222"))
      mat.setSpecularPower(60.0)
      mat
    }
    def applyTo(meshView: JMeshView): Unit = {
      val material = toPhong()
      meshView.setMaterial(material)
      val mesh = meshView.getMesh.asInstanceOf[JTriangleMesh]
      mesh.getTexCoords.setAll(colMap.textureCoords(): _*)
    }

    def toAwt(): ColorMap[java.awt.Color] = {
      var i = -1
      def colGenAwt(d: Double): java.awt.Color = {
        i += 1
        colMap.colors(i).toAwt()
      }
      new memnets.ui.ColorMapBase(colMap.max, colMap.numColors, colMap.min)(colGenAwt)
    }
  }

  implicit def sub2Option(f: Subscription): Option[Subscription] = Option(f)
  // want specific Tickable types here so DefaultSkin doesn't use specific class signature
  implicit def tfx2Option[T <: TickableFX](fx: T): Option[TickableFX] = Some(fx)
  implicit def gcfx2Option[T <: GameControlFX](fx: T): Option[GameControlFX] = Some(fx)
  implicit def keyToOption(d: KeyCode) = Option(d)
  implicit def node2opt(node: Node): Option[Node] = Option(node)
  implicit def timeline2opt[T <: Timeline](t: T): Option[Timeline] = Option(t)
  implicit class FileStringExt(val file: String) extends AnyVal {
    import memnets.utils._
    def loadFXML[T <: JNode]: T = FXMLLoader.load(file.asURL)
  }

  object LinearGradientP extends scala.AnyRef {
    def apply(
        startX: Double = 0,
        startY: Double = 0,
        endX: Double = 1,
        endY: Double = 0,
        proportional: Boolean = true,
        cycleMethod: jfxp.CycleMethod = jfxp.CycleMethod.NO_CYCLE,
        stops: Iterable[JStop]): JLinearGradient = {
      new JLinearGradient(startX, startY, endX, endY, proportional, cycleMethod, stops.toSeq: _*)
    }
    def apply(horizontal: Boolean, cycleMethod: jfxp.CycleMethod, stops: JStop*): JLinearGradient = {
      if (horizontal) new JLinearGradient(0, 0, 1, 0, true, cycleMethod, stops: _*)
      else new JLinearGradient(0, 0, 0, 1, true, cycleMethod, stops: _*)
    }
    def apply(horizontal: Boolean, stops: JStop*): JLinearGradient =
      apply(horizontal, jfxp.CycleMethod.NO_CYCLE, stops: _*)
  }
  object ColorP extends AnyRef {
    def apply(r: Double, g: Double, b: Double, alpha: Double = 1.0): JColor = new JColor(r, g, b, alpha)
  }
  implicit class ShapeContainerExt[T <: JMeshView](val container: ShapeContainer[T]) extends AnyVal {
    def extractShape(): MeshView = {
      container.getChildren.clear()
      container.getEmissiveLight.getScope.clear()
      container.getSelfIlluminationLight.getScope.clear()
      container.getShape
    }
  }
  implicit class MouseEventExt(val me: MouseEvent) extends AnyVal {
    def node = me.source.asInstanceOf[javafx.scene.Node]
    def userData = node.getUserData
  }
  implicit def scalaNodeToJNodeExt[T <: Node](n: T): JNodeExt[JNode] = new JNodeExt(n.delegate)
  implicit class JNodeExt[T <: JNode](val n: T) extends AnyVal {
    def screenPos: (Double, Double, Double, Double) = {
      val scene = n.getScene
      val win = scene.getWindow
      val nodeCoord = n.localToScene(0.0, 0.0)
      val x = Math.round(win.getX + scene.getX + nodeCoord.getX)
      val y = Math.round(win.getY + scene.getY + nodeCoord.getY)
      val bnds = n.getBoundsInLocal
      (x, y, bnds.getWidth, bnds.getHeight)
    }
    def findById[T <: JNode](id: String): T = n.lookup(if (id.startsWith("#")) id else "#" + id).asInstanceOf[T]
    def findTextById(id: String) = findById[JText](id)

    def rotateY(angle: Double): Unit = {
      import javafx.geometry._
      n.rotationAxisProperty.setValue(new Point3D(0, 1, 0))
      n.rotateProperty.set(angle)
    }

  }
  import scalafx.animation._
  import scalafx.util.Duration
  implicit def transitionToOption(t: Transition) = Some(t)
  implicit class AnimationExt[T <: Animation](val t: T) extends AnyVal {
    def playFromEnd = {
      t.delegate.setRate(-1)
      t.delegate.playFrom(t.delegate.getCycleDuration)
    }
    def playing = t.delegate.getStatus == Animation.Status.Running.delegate
  }

  def fadeInBlink(n: Node) =
    new SequentialTransition(
      node = n,
      children = Seq(
        new FadeTransition {
          duration = Duration(900)
          fromValue = 0.0
          toValue = 1.0
          cycleCount = 1
          interpolator = Interpolator.EaseBoth
        },
        new FadeTransition {
          duration = Duration(300)
          fromValue = 0.0
          toValue = 1.0
          cycleCount = 3
          interpolator = Interpolator.Linear
        }
      )
    )
  def fadeIn(n: Node, ms: Int = 900) = new FadeTransition {
    node = n
    duration = Duration(ms)
    fromValue = 0.0
    toValue = 1.0
    cycleCount = 1
    interpolator = Interpolator.EaseBoth
  }
  def fadeOut(n: Node, ms: Int = 700, endOpacity: Double = 0.1) = new FadeTransition {
    node = n
    duration = Duration(ms)
    fromValue = 1.0
    toValue = endOpacity
    cycleCount = 1
    interpolator = Interpolator.EaseBoth
  }
  def pauseBlink(
      n: Node = null,
      pauseMs: Int = 200,
      endOpacity: Double = 0.5,
      onStart: => Any = {},
      onSecond: => Any = {}
  ) = new SequentialTransition {
    node = n
    cycleCount = 1
    children = Seq(
      new PauseTransition {
        duration = Duration(pauseMs)
        onFinished = e => {
          n.opacity.value = 0.0
          onStart
        }
      },
      new FadeTransition {
        duration = Duration(400)
        fromValue = endOpacity
        toValue = 1.0
        cycleCount = 2
        autoReverse = true
        interpolator = Interpolator.EaseBoth
        onFinished = e => {
          onSecond
        }
      },
      new FadeTransition {
        duration = Duration(400)
        fromValue = endOpacity
        toValue = 1.0
        cycleCount = 2
        autoReverse = true
        interpolator = Interpolator.EaseBoth
      }
    )
  }
  def blinker(n: Node = null, dur: Int = 1000, maxOpacity: Double = 1.0) = new FadeTransition {
    node = n
    duration = Duration(dur)
    fromValue = 0.0
    toValue = maxOpacity
    interpolator = Interpolator.EaseBoth
    cycleCount = Timeline.Indefinite
    autoReverse = true
  }

  def runLaterP(op: => scala.Any): Unit = {
    if (Platform.isFxApplicationThread)
      op
    else
      Platform.runLater(op)
  }
  @throws[InterruptedException]
  @throws[ExecutionException]
  def runAndWait(op: => scala.Any): Unit = {
    val r: Runnable = () => op
    runAndWait(r)
  }
  @throws[InterruptedException]
  @throws[ExecutionException]
  def runAndWait(r: Runnable): Unit = {
    if (!Platform.isFxApplicationThread) {
      logger.debug("not FX thread")
      val future = new FutureTask(r, null) {
        override def done(): Unit = {
          super.done()
          logger.trace("task done called")
          try {
            if (!isCancelled) get
          } catch {
            case ee: ExecutionException =>
              // Exception occurred, deal with it
              logger.error("TaskExecuteException", ee)
              throw ee
            case ie: InterruptedException =>
              // Shouldn't happen, we're invoked when computation is finished
              throw new AssertionError(ie)
          }
        }
      }
      Platform.runLater(future)
      logger.trace("waiting for get")

      try {
        future.get
      } catch {
        case ie: InterruptedException =>
          logger.warn("task interrupted", ie)
        case th: Throwable =>
          throw th
      }
    } else {
      logger.debug("FX thread")
      r.run()
    }
  }
  @throws[InterruptedException]
  @throws[ExecutionException]
  def runAndWait[T](callable: Callable[T]): T = {
    if (!Platform.isFxApplicationThread) {
      logger.debug("not FX thread: callable")
      val future = new FutureTask(callable)
      Platform.runLater(future)
      future.get
    } else {
      logger.debug("FX thread: callable")
      callable.call()
    }
  }

  /** Asks the user to choose a plain file.
   *
   * @param title What to put in the file chooser dialog title bar.
   * @return The chosen file.
   */
  def open(
      stage: Stage,
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File] = {

    val chooser = new FileChooser()
    chooser.title = title
    val filter = new ExtensionFilter(desc, ext.toSeq)
    chooser.extensionFilters.setAll(filter)
    chooser.selectedExtensionFilter = filter
    initFile match {
      case Some(file) =>
        chooser.initialDirectory = if (file.isFile) file.getParentFile else file
      case None =>
        chooser.initialDirectory = "."

    }
    Option(chooser.showOpenDialog(stage))
  }
  def save(
      stage: Stage,
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File] = {

    val chooser = new FileChooser()
    chooser.title = title
    val filter = new ExtensionFilter(desc, ext.toSeq)
    chooser.extensionFilters.setAll(filter)
    chooser.selectedExtensionFilter = filter
    initFile.foreach { x =>
      chooser.initialDirectory = x.getParentFile
      chooser.initialFileName = x.getName
    }
    Option(chooser.showSaveDialog(stage))
  }

  // Sound
  private val _mediaPlayers = ArrayBuffer[MediaPlayer with MediaSound]()

  implicit class MediaPlayerExt(val mp: MediaPlayer) extends AnyVal {
    def quietPlay(): Unit = {
      mp.mute = true
      mp.play()
    }
    def quietStop(): Unit = {
      mp.mute = true
      mp.stop()
    }
  }

  implicit class MediaFileExt(val f: String) extends AnyVal {
    def toMediaPlayer(volume: Double = 1.0, soundTrack: Boolean = true) = {
      val mp = new MediaPlayer(new Media(f.asURL.toExternalForm)) with MediaSound {
        val isSoundTrack = soundTrack
      }
      _mediaPlayers += mp
      mp.mute = true
      mp.volume = volume // NOTE : repeated calls to volume are multiplied together and ends up at 0.0...
      mp.cycleCount = MediaPlayer.Indefinite
      mp.onPlaying = {
        mp.mute.value = false
      }
      mp
    }
  }

  /**
   * NOTE : only destroys mediaPlayer sounds.
   */
  def destroyTracks(): Unit = {
    for (mp <- _mediaPlayers) mp.dispose()
  }
  def startTracks(): Unit = {
    for (mp <- _mediaPlayers if mp.isSoundTrack) mp.quietPlay()
  }
  def stopTracks(): Unit = {
    for (mp <- _mediaPlayers) mp.quietStop()
  }
}
