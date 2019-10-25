package memnets.fx

import scalafx.scene._
import scalafx.scene.canvas.Canvas
import scalafx.scene.effect.BlendMode
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

object FadingSkinFX {
  val DEFAULT_FADE_COLOR = Color.color(0.0, 0.0, 0.0, 0.3)
}
trait FadingSkinFX extends SkinFX {
  import FadingSkinFX._
  var fadeColor = DEFAULT_FADE_COLOR
  abstract override def initCanvas(elemPane: Pane, canvas: Canvas): List[Node] = {
    val list = super.initCanvas(elemPane, canvas)
    canvas.blendMode = BlendMode.Add
    list
  }
  override def clearCanvas(canvas: Canvas) = {
    val del = canvas.delegate
    if (del.visibleProperty.get) {
      val gc = del.getGraphicsContext2D
      gc.setFill(fadeColor)
      gc.fillRect(0.0, 0.0, del.widthProperty.get, del.heightProperty.get)
    }
  }
}
