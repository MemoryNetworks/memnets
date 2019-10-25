package memnets.fx

import memnets.linalg.W
import memnets.model._
import scalafx.geometry.Pos
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape._
import scalafx.scene.text._
import scalafx.scene._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GraphSkinFX extends SkinFX with Logging {
  name = "Graph"
  backImageOn = false
  backColor = Colorf.WHITE
  topPaneOn = true

  /** alternative method to create entire model at once */
  override def createSystem(system: DynamicSystem): Iterable[TickableFX] = {
    val tfx = new ListBuffer[TickableFX]()
    val id2var = new mutable.HashMap[Int, NodeFX]()
    for (v <- system.variables)
      id2var(v.id) = new NodeFX(v)

    implicit def id2fx(id: Int): NodeFX = id2var(id)

    val edgesFX = new EdgeGroupFX
    val wScale = edgeScale

    for (w <- system.sparse.matrix.weights if !w.isLoop) {
      val efx = new EdgeFX(w.src, w.tgt, w)
      efx.strokeWidth = wScale * 7.0
      edgesFX.children.add(efx)
    }
    tfx += edgesFX
    tfx ++= id2var.values // add last to on top
    tfx
  }
  override def createY(y: Y) = None
  override def isSuitable(system: DynamicSystem): Boolean = {
    system.layers.length == 0 && system.variables.length < 256
  }

  protected val negEdgeDash = List(java.lang.Double.valueOf(10.0), java.lang.Double.valueOf(15.0))

  /** put all edges in cached group.  */
  protected class EdgeGroupFX extends Group with TickableFX {
    cache = true
    def node: Option[Node] = this
    def tick(te: Tick): Unit = {}
  }
  protected val nodeFont = Font("Tahoma", FontWeight.Bold, 22)
  val w = 80.0
  val h = 60.0
  protected class EdgeFX(val src: NodeFX, val tgt: NodeFX, val w: W) extends QuadCurve {
    startX = src.centerX
    startY = src.centerY
    endX = tgt.centerX
    endY = tgt.centerY
    val (ctrX, ctrY) = control(this)
    controlX = ctrX
    controlY = ctrY
    stroke = yColorMap(src.ys)
    fill = null
    if (w.w < 0.0)
      strokeDashArray = negEdgeDash
  }
  protected class NodeFX(val ys: Y) extends TickableFX {
    val fx = new StackPane {
      userData = ys
      prefWidth = w
      prefHeight = h
      cache = true
      val circle = new Ellipse {
        radiusX = w / 2.0
        radiusY = h / 2.0
        strokeWidth = 2.0
        stroke = Color.Black
        fill = new Color(GraphSkinFX.this.backColorConverted)
      }
      val text = new Text {
        font = nodeFont
        text = ys.description
        fill = yColorMap(ys)
        strokeWidth = 1.0
        stroke = Color.Black
        smooth = true
      }
      children = List(circle, text)
      StackPane.setAlignment(circle, Pos.Center)
      StackPane.setAlignment(text, Pos.Center)
    }
    fx.relocate(ys.ui.loc.x - w / 2.0, ys.ui.loc.y - h / 2.0)

    def centerX: Double = fx.layoutX.value + fx.circle.radiusX.value
    def centerY: Double = fx.layoutY.value + fx.circle.radiusY.value
    def node = fx
    def tick(te: Tick): Unit = {
      val mag = Math.abs(ys.act)
      val opac = if (mag > 0.01) Math.min(1.0, mag / 3.0) else 0.0
      fx.text.delegate.setOpacity(0.6 * opac + 0.4)
    }
    override def findTarget(x0: Double, y0: Double) = Some(UserSource(ys, loc = ys.ui.loc, h = h, xoff = w / 2.0))
  }
  protected def control(quad: QuadCurve): (Double, Double) = {
    import quad._
    val diffX = endX.value - startX.value
    val diffY = endY.value - startY.value
    val rad = Math.atan2(diffY, diffX) - Math.PI / 12.0 // 15 degrees
    val a = 80.0
    (startX.value + a * Math.cos(rad), startY.value + a * Math.sin(rad))
  }
}
