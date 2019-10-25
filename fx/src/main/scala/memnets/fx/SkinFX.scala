package memnets.fx

import java.util.function.Consumer

import memnets.fx.fx3d._
import memnets.model._
import memnets.ui._
import scalafx.scene.Node
import scalafx.scene.canvas.Canvas
import scalafx.scene.effect._
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

object SkinFX extends Logging {
  def apply(init: Consumer[BatterySkinFX] = sk => {}): SkinFX = {
    val skin = new BatterySkinFX()
    init.accept(skin)
    skin
  }
}

abstract class SkinFX extends SkinFXBase {
  type UI = TickableFX
  var customPanel: Option[TickableFX] = None
  def initStyle(pane: Pane, timePane: Pane): Unit = {
    val i = backImage
    var css = style
    if (css.isEmpty) {
      if (backImageOn) {
        for (backImg <- imageLookup(i)) {
          logger.debug("found image: " + backImg)
          css = s"-fx-background-image: url($backImg); -fx-background-size: stretch;"
        }
      } else
        css = s"-fx-background-color: ${backColorConverted.toRGBA};"
    }
    for (parentPane <- pane.parent.value.as[JPane] if backBlend != javafx.scene.effect.BlendMode.SRC_OVER) {
      logger.debug("coloring parent pane")
      parentPane.setStyle(s"-fx-background-color: ${backColorConverted.toRGBA};")
    }
    pane.blendMode.delegate.setValue(backBlend)
    pane.style = css
    timePane.style = timeChartLookup(i).getOrElse(" -fx-background-color: rgba(0,0,0,0.5);")
  }

  /**
   * default is to put canvas behind elements
   */
  def initCanvas(elemPane: Pane, canvas: Canvas): List[Node] = {
    canvas.visible = true
    canvas.width.value = Display.width
    canvas.height.value = Display.height
    canvas.delegate.setBlendMode(canvasBlend)
    canvas.effect = createEffect(canvasEffect)
    RenderContextFX._gc = canvas.graphicsContext2D
    List(canvas, elemPane)
  }
  def clearCanvas(canvas: Canvas): Unit = {
    val del = canvas.delegate
    if (del.visibleProperty.get)
      del.getGraphicsContext2D.clearRect(0.0, 0.0, del.widthProperty.get, del.heightProperty.get)
  }

  override def createHelper(elem: Element) = {
    elem match {
      case d3: Scene3DFX   => create3d(d3)
      case tfx: TickableFX => Some(tfx)
      case default         => None
    }
  }
  override def createTracer(tc: Tracer): Option[TickableFX] = TracerFX(tc, yColorMap(tc.tgt))
  override def createSignal(s: Signal): Option[TickableFX] = {
    // non-realtime signals on layers don't display quite right.  hard to fix, so filter out
    val yOpt = s.tgt.as[Y]
    // getting hackish.  yikes...
    if (signalsOn &&
        (s.isUser || (yOpt.isDefined && yOpt.get.ui.isShown)) &&
        s.tgt.layer.system.variablesShown < sparseLayer)
      SignalFX(s, yColorMap)
    else
      None
  }
  // special case where want default behavior, but can override
  override def create3d(scene3d: Scene3D): Option[TickableFX] = {
    var fx: TickableFX = null
    for (sc3d <- scene3d.as[Scene3DFX]) {
      fx = if (sc3d.showFrame) {
        val container = new ContainerFX(sc3d)
        if (!sc3d.showGlass) container.frame.hideGlass()
        if (sc3d.showBorder) container.frame.showBorder()
        container
      } else
        sc3d
    }
    fx
  }
  override def createMesh(grid: GridData, meshType: MeshType): Option[TickableFX] = {
    val sc3d = new Scene3DFX(
      loc = grid.ui.loc,
      w = grid.hints.width3D,
      h = grid.hints.height3D,
      zoom = grid.hints.zoom3D,
      showGlass = grid.hints.showGlass,
      showBorder = grid.hints.showBorder
    )
    sc3d.sun.translateY = 1000.0
    sc3d.sun.color = Color.web("#aaa")
    sc3d.ambientLight.color = Color.web("#eee")
    sc3d.subScene3d.zoom.value = 2.0
    val subGrid = grid match {
      case sl: Sliding =>
        grid.subGridData(sl.rows, safeMeshDivs)
      case default =>
        grid.subGridData(safeMeshDivs, safeMeshDivs)
    }
    sc3d += new Mesh3DFX(subGrid, meshType, colorMap = colorMap)
    create3d(sc3d)
  }
  def createEffect(effectType: EffectType): Effect = {
    effectType match {
      case EffectType.None =>
        null
      case EffectType.Blur =>
        new BoxBlur(width = 5.0, height = 5.0, iterations = 3)
      case EffectType.DropShadow =>
        new DropShadow(radius = 0.5, offsetX = 1.0, offsetY = 1.0, Color.Black.opac(0.6))
      case EffectType.Lighting =>
        val lght = new Lighting()
        lght.surfaceScale = 5.0
        lght
      case EffectType.Custom =>
        canvasEffectCustom
    }
  }
  def canvasEffectCustom: Effect = null
}
