package memnets.fx

import memnets.model._
import memnets.ui.{FullScene, YColorMap}
import scalafx.scene._
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.Pane

object FullSceneSkinFX {
  def apply(fullScreenFX: FullSceneFX, prior: SkinFX): FullSceneSkinFX = {
    val skin = new FullSceneSkinFX(fullScreenFX, prior)
    skin.topPaneOn = prior.topPaneOn
    skin.chartOn = prior.chartOn
    skin
  }
}
class FullSceneSkinFX(val fullSceneFX: FullSceneFX, val prior: SkinFX) extends SkinFX with FullScene[Node, Col] {
  require(prior != null)

  name = "FullScene"
  customClock = prior.customClock
  customTimeChart = prior.customTimeChart
  useElementDpad = prior.useElementDpad
  customPanel = prior.customPanel
  titleOn = prior.titleOn
  topPaneOn = prior.topPaneOn
  chartGlassOn = prior.chartGlassOn
  chartDynamic = prior.chartDynamic
  chartSampling = prior.chartSampling
  chartOn = prior.chartOn

  override def yColorMap: YColorMap[Col] = prior.yColorMap
  override def initStyle(pane: Pane, timePane: Pane): Unit = {
    super.initStyle(pane, timePane)
    var css = prior.fullScreenStyle
    if (css.isEmpty) {
      for {
        fs <- fullSceneFX.as[FrameFXStyle]
        pstyle <- fs.paneStyle
      } {
        logger.debug("found paneStyle")
        css = pstyle
      }
    }
    if (css.isEmpty)
      css = s"-fx-background-color: rgba(10,10,10,1.0);"

    pane.style = css
    timePane.style = s" -fx-background-color: rgba(0,0,0,${fullSceneFX.headerOpacity.getOrElse(0.3)});"
  }
  // do NOT call prior.init.  fullScene assumes already built.  no edits allowed.
  override def init(model: DynamicSystem): Unit = {
    // prior.init(model)
  }

  /**
   * mainly here for games.
   * in fullScene, scene already built, but still want dynamic +/- of goals/signals
   */
  override def create(elem: Element): Option[TickableFX] = elem match {
    case g: Goal =>
      prior.create(g)
    case sig: Signal =>
      prior.create(sig)
    case default =>
      None
  }
  def createY(y: Y): Option[TickableFX] = None
  override def remove(elem: Element): Unit = prior.remove(elem)

  override def initCanvas(elemPane: Pane, canvas: Canvas): List[Node] = {
    canvas.visible = false
    canvas.effect = null
    RenderContextFX._gc = canvas.graphicsContext2D
    List(elemPane)
  }
  override def clearCanvas(canvas: Canvas): Unit = {
    // should be a NOP since canvas not visible, BUT special case for Skin3DFXAdapter
    prior.clearCanvas(canvas)
  }
}
