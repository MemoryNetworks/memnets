package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import memnets.ui._
import scalafx.scene._
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.Pane
import scalafx.scene.transform._

/**
 * unlike other Skins, this skin has state!
 * you must call createNetwork first
 * all subsequent calls to createX...  will forward to one global Scene3D/Skin3DFX
 */
class Skin3DFXAdapter(val skin3d: Skin3DFX = new BatterySkin3DFX()) extends SkinFX with Logging {
  protected var sc3d: Scene3DFX = _

  name = skin3d.name
  useElementDpad = true

  override def imageLookup(i: SkinImage): Option[String] = None // no background by default
  override def systemBuilt(): Unit = {
    skin3d.systemBuilt()
    sc3d.init()
    if (sc3d.originLight.isDefined) {
      logger.debug("origin light turn off")
      sc3d.originLight.get.lightOn = false
    }
  }
  override def createSystem(system: DynamicSystem): Iterable[UI] = {
    for (t3d <- skin3d.createSystem(system))
      sc3d.add(t3d)

    val container = new ContainerFX(sc3d)
    if (!sc3d.showGlass) container.frame.hideGlass
    if (sc3d.showBorder) container.frame.showBorder
    List(container)
  }
  override def init(model: DynamicSystem): Unit = {
    sc3d = skin3d.createSingleton3d(model)
    skin3d.init(model)
    gradientHints = skin3d.gradientHints
  }
  override def initStyle(pane: Pane, timePane: Pane): Unit = {
    backImageOn = false
    backColor = Colorf.BLACK
    super.initStyle(pane, timePane)
  }
  override def yColorMap = skin3d.yColorMap
  override def create(elem: Element): Option[TickableFX] = {
    elem match {
      case pp: PhasePlot =>
        if (!pp.phases.isEmpty) {
          if (pp.originLight) sc3d.addOriginLight()
          sc3d.autoCamera = pp.autoCamera
          val spacing = 80.0
          val startZ = spacing * (pp.phases.size - 1) / 2.0
          logger.trace("startZ = " + startZ)
          for ((ph, i) <- pp.phases.zipWithIndex) {
            val fx = new Phase3DFX(ph, yColorMap)
            fx.group.transforms = List(
              new Rotate(-pp.sceneRotateY, axis = Rotate.YAxis),
              new Rotate(pp.sceneRotateZ, axis = Rotate.ZAxis),
              new Translate(x = 0, y = -ph.translateX, z = startZ - i * spacing))
            sc3d += fx
          }
          pp.onCreateScene(sc3d)
        }
      case default =>
        val opt = skin3d.create(elem)
        if (opt.isDefined)
          sc3d += opt.get
    }
    None
  }
  def createY(y: Y): Option[TickableFX] = None
  override def layout(model: DynamicSystem) = skin3d.layout(model)
  override def initCanvas(elemPane: Pane, canvas: Canvas): List[Node] = {
    canvas.visible = false
    canvas.effect = null
    List(elemPane)
  }
  override def clearCanvas(canvas: Canvas) = skin3d.clearCanvas()
  override def remove(elem: Element): Unit = {
    elem match {
      case sig: Signal =>
        sc3d.removeE(sig)
      case yg: YGoal =>
        sc3d.removeE(yg)
      case default =>
    }
  }
  override def toString: String = s"Skin3DFXAdapter[Skin3DFX= $skin3d]"
}
