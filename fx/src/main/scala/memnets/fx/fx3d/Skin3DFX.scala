package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import memnets.ui._
import scalafx.scene.Group
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.shape.StrokeLineCap
import scalafx.scene.transform.Rotate

object Skin3DFX {
  def apply(fx3d: Skin3DFX = new BatterySkin3DFX): SkinFX = new Skin3DFXAdapter(fx3d)
}

abstract class Skin3DFX extends SkinFXBase {
  type UI = Tickable3DFX
  useElementDpad = true
  object canvas extends Canvas {
    managed = false
    mouseTransparent = true
    translateX <== -width / 2.0
    translateY <== -height / 2.0
    rotate = 90.0
    rotationAxis = Rotate.XAxis
  }
  Display.onChange { res =>
    canvas.width.value = res.width
    canvas.height.value = res.height
  }
  protected val gc = canvas.graphicsContext2D
  protected var canvasImage: Option[Image] = None
  var showCanvas3d = true

  override def init(system: DynamicSystem): Unit = {
    super.init(system)
    logger.debug("skin3d init")
    // using match just to make more readable
    canvasImage = backImageOn match {
      case true =>
        imageLookupURL(backImage).map(x => new Image(x.openStream()))
      case false =>
        None
    }
  }
  override def imageLookup(i: SkinImage): Option[String] = {
    import SkinImage._
    val back: String = i match {
      case ZERO | ONE  => "tiles-background-3dv0.jpg"
      case TWO | THREE => "tiles-background-3dv1.jpg"
      case FOUR        => "tiles-background-3dv2.jpg"
      case FIVE | SIX  => "tiles-background-3dv3.jpg"
      case default     => null
    }
    if (back == null) None else Some("/backgrounds3d/" + back)
  }

  override def zoomDefault(system: DynamicSystem): Double = 1.0
  def createSingleton3d(system: DynamicSystem): Scene3DFX = {
    val sc3d = new Scene3DFX(
      loc = Loc().down(65),
      w = 800,
      h = 450,
      tiltDown = 45.0,
      //   cameraZ = -2000.0,
      showBorder = true,
      showGlass = false
    )
    if (showCanvas3d)
      sc3d.addContent(new Group() {
        translateY = -1.0
        children.add(canvas)
      }.delegate)
    sc3d
  }
  def clearCanvas(): Unit = {
    if (showCanvas3d) {
      val del = canvas.delegate
      gc.clearRect(0.0, 0.0, del.widthProperty.get, del.heightProperty.get)
      gc.lineCap = StrokeLineCap.Round
      if (canvasImage.isDefined)
        gc.drawImage(canvasImage.get, 0.0, 0.0, del.getWidth, del.getHeight)
      else {
        gc.fill = backColor
        gc.fillRect(0.0, 0.0, del.getWidth, del.getHeight)
      }
    }
  }
}
