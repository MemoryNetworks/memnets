package memnets.fx.fx3d

import com.sun.javafx.geom.Vec3d
import memnets.fx._
import memnets.fx.fx3d.Comet3DFX.Ball
import memnets.model._
import memnets.ui._
import scalafx.scene.Group
import scalafx.scene.transform._

object PhasePlot3DFX extends Logging {
  def apply(pp: PhasePlot, yColorMap: YCol): Scene3DFX = {
    val sc3d = new Scene3DFX(
      loc = pp.loc,
      w = pp.width,
      h = pp.height,
      tiltDown = pp.tiltDown,
      zoom = pp.zoom,
      rotate = pp.sceneRotateY,
      showFrame = pp.showFrame,
      showGlass = pp.showGlass,
      showBorder = pp.showBorder
    )
    if (pp.originLight) sc3d.addOriginLight()
    sc3d.name = "PhasePlot"
    sc3d.autoCamera = pp.autoCamera
    sc3d.subScene3d.paneStyle = pp.paneStyle
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
    sc3d
  }
}
class Phase3DFX(phase2D: Phase2D, yColorMap: YCol) extends Tickable3DFX with Logging {
  val transform = new Group
  transform.managed = false
  transform.transforms.add(new Rotate(-90.0, Rotate.XAxis))
  val group = new Group()
  group.managed = false
  transform.children.add(group)
  val axes = new Axes3DFX(showLabels = phase2D.showLabels, length = 300)
  import axes._
  object comet extends Comet3DFX(scale = phase2D.scale, window = phase2D.window) {
    @inline final override def tickHelper(i: Int, pt: Vec3d, sp: Ball): Unit = {
      val del = sp.phong.delegate
      if (i % 7 != 0)
        del.diffuseColorProperty.set(if (pt.x > 0.0) xAxis.posColor else xAxis.negColor)
      else
        del.diffuseColorProperty.set(if (pt.y > 0.0) yAxis.posColor else xAxis.negColor)
    }
  }
  if (phase2D.showAxis)
    group.children.add(axes)

  group.children.add(comet)
  def node = transform
  val ph3D: Option[Phase3D] = phase2D match {
    case ph3: Phase3D =>
      Some(ph3)
    case default =>
      None
  }
  override def init(): Unit = {
    logger.trace("init")
    xAxis.color(yColorMap.trackColor(phase2D.x))
    yAxis.color(yColorMap.trackColor(phase2D.y))
    if (ph3D.isDefined) {
      zAxis.color(yColorMap.trackColor(ph3D.get.z))
      zAxis.visible = true
      comet.temporal = false
    } else {
      zAxis.visible = false
      comet.temporal = phase2D.temporal
    }
    super.init()
  }
  override def reset() = comet.reset()
  phase2D.onDirty = {
    logger.trace("dirty: re-init")
    init()
  }
  def tick(te: Tick): Unit = {
    import phase2D._
    if (te.t % sampling == 0)
      comet.tick(x.act, y.act, if (ph3D.isDefined) ph3D.get.z.act else 0.0)
  }
}
