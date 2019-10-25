package memnets.fx.fx3d

import javafx.scene.shape.TriangleMesh
import memnets.fx._
import memnets.model._
import memnets.ui._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.transform._

class Mesh3DFX(
    val grid: GridData,
    val meshType: MeshType,
    val colorMap: ColMap = HeatMapFX(),
    val scale: Double = 5.0
) extends Tickable3DFX
    with Logging {

  val mesh = new TriangleMesh()
  val meshView = new MeshView(mesh) {
    drawMode = if (meshType == MeshType.Fill) DrawMode.Fill else DrawMode.Line
    pickOnBounds = true
    focusTraversable = false
    managed = false
    rotate = -90.0
    // NOTE : don't scale mesh b/c jitters when interacts w/ group transforms
    depthTest = DepthTest.Enable
  }
  meshView.setCullFace(CullFace.None) // don't set inside of class....
  colorMap.applyTo(meshView)

  val group = new Group {
    transforms = List(new Rotate(-90.0, Rotate.XAxis), new Scale(scale, scale, scale))
    children = meshView
    managed = false
    pickOnBounds = true
  }
  val node = group
  val surface = new MeshSurface(grid, colorMap)

  override def init(): Unit = {
    super.init()
    val smooth = surface.faceSmoothingGroups()
    mesh.getFaceSmoothingGroups.setAll(smooth, 0, smooth.length)
  }
  override def reset(): Unit = {
    super.reset()
    grid.reset()
  }
  final def tick(te: Tick): Unit = {
    if (grid.preRender(te)) {
      surface.tick(te)
      mesh.getFaces.setAll(surface.faceArray, 0, surface.faceArray.length)
      mesh.getPoints.setAll(surface.pointArray, 0, surface.pointArray.length)
    }
  }
}
