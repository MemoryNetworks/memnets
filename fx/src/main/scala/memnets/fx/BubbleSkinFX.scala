package memnets.fx

import java.lang.Math._

import memnets.model._
import scalafx.scene._
import scalafx.scene.image._
import scalafx.scene.paint._

object BubbleSkinFX {
  def apply(): BubbleSkinFX = new BubbleSkinFX
}
class BubbleSkinFX extends BatterySkinFX {
  name = "Bubble"
  backImageOn = false
  object BubbleFX {
    val r = 25.0
    val h = 2.0 * r
    val magic = 2.0
    val ui: JGroup = "bubble.fxml".loadFXML
    val root = new Group(ui)
    val tempScene = new Scene(
      width = 200,
      height = 200,
      depthBuffer = true,
      antiAliasing = SceneAntialiasing.Balanced
    ) // magic # ???
    tempScene.content = root
    val params = new SnapshotParameters()
    params.fill = Color.Transparent
    // hack : w/o "warm-up" 1st guy has bad bounds
    root.snapshot(params, new WritableImage(200, 200))
    root.setScaleX(2.0)
    root.setScaleY(2.0)
    def takeSnapshot(y: Y): Image = {
      val bnds = root.boundsInParent.value
      val w = bnds.getWidth.asInstanceOf[Int]
      val h = bnds.getHeight.asInstanceOf[Int]
      root.snapshot(params, new WritableImage(w, h))
    }
  }

  sealed class BubbleFX(ys: Y) extends ImageView with TickableFX {
    import BubbleFX._
    pickOnBounds = true
    def node = this
    val posEdgeCol = yColorMap(ys)
    val negEdgeCol = posEdgeCol.darker.darker.darker
    val posCol = RadialGradient(
      0.0,
      0.0,
      0.5,
      0.5,
      0.9,
      true,
      CycleMethod.NoCycle,
      Stop(0.0, posEdgeCol),
      Stop(1.0, Color.Transparent))
    val negCol = RadialGradient(
      0.0,
      0.0,
      0.5,
      0.5,
      0.9,
      true,
      CycleMethod.NoCycle,
      Stop(0.0, posEdgeCol.interpolate(Color.Black, 0.5)),
      Stop(1.0, Color.Transparent))
    var yScale = YRange.scale
    override def init(): Unit = {
      yScale = ys.ui.scale.getOrElse(YRange.scaleF).toDouble
      val loc = ys.ui.loc
      image = takeSnapshot(ys)
      scaleX.value = 0.5
      scaleY.value = 0.5
      val b = this.getBoundsInParent
      relocate(loc.x - b.getWidth, loc.y - 1.5 * b.getHeight)
    }
    def tick(te: Tick): Unit = {
      val gc = RenderContextFX.gc
      val fade = (te.t) % 3 == 0
      val act = ys.act
      var barCol = posCol
      var edgeCol = posEdgeCol
      var render = false
      if (act > 0.01)
        render = true
      else if (act < -0.01) {
        render = true
        barCol = negCol
        edgeCol = negEdgeCol
      }
      if (render) {
        if (fade) {
          val opac = opacity.value + 0.05
          opacity = if (opac > 1.0) 1.0 else 1.2 * opac
        }
        val mag = abs(act)
        // side effects on bubble    gc.globalBlendMode = BlendMode.COLOR_BURN
        renderEdges(te, ys, mag, r, edgeCol, gc)

        var scale = 1.0
        var alpha = mag / yScale
        if (alpha > 1.0) {
          scale = scale + (alpha - 1.0) * 0.5
          alpha = 1.0
        }
        scaleX.value = scale * 0.5
        scaleY.value = scale * 0.5
        gc.globalAlpha = alpha
        gc.fill = barCol
        gc.fillOval(
          ys.ui.loc.x - r * scale,
          ys.ui.loc.y - 2.0 * r - (scale - 1.0) * r,
          2.0 * r * scale,
          2.0 * r * scale)
      } else if (fade) {
        val opac = opacity.value
        opacity = if (opac <= 0.2) 0.2 else opac * 0.7
      }
    }
    override def findTarget(x0: Double, y0: Double) =
      Some(
        new UserSource(
          ys,
          loc = ys.ui.loc,
          h = 2.0 * BubbleFX.h,
          scale = ys.ui.scale.getOrElse(10.0f).toDouble,
          xoff = r,
          zoom = 2.0))
  }
  override def createY(y: Y) = new BubbleFX(y)
}
