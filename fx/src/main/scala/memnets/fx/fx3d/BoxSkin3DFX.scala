package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import memnets.ui._
import scalafx.scene.paint.Color

object BoxSkin3DFX {
  def apply(name: String = "Box3D", colorMap: Option[ColMap] = None, showChart: Boolean = false) = {
    object skin extends BatterySkinFX with BoxSkin3DFX
    skin.layerVizType = LayerVizType.Grid
    skin.gridVizType = GridVizType.Mesh
    skin.chartOn = showChart
    skin.name = name
    skin.colorMap = colorMap
    skin
  }
}
trait BoxSkin3DFX extends Logging { self: SkinFX =>
  override def createMesh(g: GridData, meshType: MeshType): Option[TickableFX] = {
    val sc3d = new Scene3DFX(loc = g.ui.loc, w = 500, h = 500, zoom = 1.5)
    sc3d.sun.translateY = 1000.0
    sc3d.sun.color = Color.web("#777")
    sc3d.ambientLight.color = Color.web("#eee")
    sc3d += new Box3DFX(grid = g.subGridData(safeMeshDivs, safeMeshDivs), colorMap = colorMap)
    create3d(sc3d)
  }
}
