package memnets.fx.fx3d

import javafx.embed.swing.SwingFXUtils
import javafx.fxml.FXMLLoader
import memnets.awt._
import memnets.fx._
import memnets.model._
import memnets.ui._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.image._
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.shape._
import scalafx.scene.text.Font
import scalafx.scene.transform._

class BatterySkin3DFX extends Skin3DFX with Logging {
  name = "Battery3D"
  backImageOn = true
  backColor = Colorf.LIGHTBLACK
  import Textures._
  override def init(model: DynamicSystem): Unit = {
    super.init(model)
    BatterySnapshot.init()
  }
  override def createSingleton3d(model: DynamicSystem): Scene3DFX = {
    val sc3d = super.createSingleton3d(model)
    sc3d.ambientLight.color = Color.web("#ccc")
    sc3d.sun.color = Color.White
    sc3d
  }
  def createY(y: Y) = new Battery3DFX(y)
  override def createImage(grid: GridData) = new Image3DFX(grid, colorMapAwt)
  override def createMesh(grid: GridData, meshType: MeshType) = {
    val subGrid = grid match {
      case sl: Sliding =>
        grid.subGridData(sl.rows, safeMeshDivs)
      case default =>
        grid.subGridData(safeMeshDivs, safeMeshDivs)
    }

    val meshFX = new Mesh3DFX(subGrid, meshType, colorMap = colorMap)
    meshFX.group.transforms.clear()
    val scale = 2.0 * meshFX.scale
    val w = ((meshFX.surface.grid.h + 2.0) * scale) / 2.0
    meshFX.group.transforms = List(
      new Translate(0.0, w),
      new Scale(scale, scale, scale)
    )

    meshFX
  }
  override def createPlot(l: Plot) = new Plot3DFX(l)
  override def toString: String = name + "[]"

  protected class Plot3DFX(val pl: Plot) extends Group with Tickable3DFX with Logging {
    import BatterySnapshot.r
    def node = this
    override def init(): Unit = { layImg.init() }
    def tick(te: Tick): Unit = {
      layImg.tick()
      SwingFXUtils.toFXImage(layImg.bufferedImage, image)
      renderLayerLinks(gc, layImg, 20.0, 3.0, 2.0 * r, dotWeightsVisible, font)
    }
    val font = Font("Tahoma", 13)
    val w = pl.width
    val h = pl.height
    val layImg = new PlotAwt(pl, w.toInt, h.toInt, yColorMap, edgeScale)
    val image = new WritableImage(width = w.toInt, height = h.toInt)
    val plot = new ImageView(image)
    plot.rotate = 180.0
    plot.rotationAxis = Rotate.XAxis
    plot.fitHeight = h
    plot.fitWidth = w
    var border = 2.0
    val leftBorder = new Box(width = border, height = h, depth = border)
    val rightBorder = new Box(width = border, height = h, depth = border)
    val topBorder = new Box(width = w + 2 * border, height = border, depth = border)
    topBorder.translateX = w / 2
    topBorder.translateY = h + border / 2

    leftBorder.translateX = -border / 2
    leftBorder.translateY = h / 2
    rightBorder.translateX = w + border / 2
    rightBorder.translateY = h / 2
    translateX = -Display.width / 2 + pl.loc.x - w / 2
    translateZ = -Display.height / 2 + pl.loc.y
    children ++= List(leftBorder, plot, topBorder, rightBorder)

    Textures(METAL, leftBorder)
    Textures(METAL, rightBorder)
    Textures(METAL, topBorder)
  }

  /**
   * NOTE : size is magic # based on ImageSource.fxml w x h
   */
  class Image3DFX(val grid: GridData, colMap: ColorMap[java.awt.Color]) extends Group with Tickable3DFX with Logging {
    val imageAwt = new ImageAwt(grid, colMap)
    val w = grid.hints.width
    val h = grid.hints.height
    def node = this
    override def reset(): Unit = {
      super.reset()
      imageAwt.reset()
    }
    def tick(te: Tick): Unit = {
      if (grid.preRender(te)) {
        imageAwt.tick(te)
        SwingFXUtils.toFXImage(imageAwt.bufferedImage, image)
      }
    }
    val image = new WritableImage(
      width = imageAwt.bufferedImage.getWidth,
      height = imageAwt.bufferedImage.getHeight
    )
    val plot = new ImageView(image)
    plot.opacity = grid.hints.opacity
    plot.rotate = 180.0 // images are upside down in 3D by default
    plot.rotationAxis = Rotate.XAxis
    plot.fitWidth.value = w
    plot.fitHeight.value = h

    var border = 2.0
    val leftBorder = new Box(width = border, height = h, depth = border)
    val rightBorder = new Box(width = border, height = h, depth = border)
    val topBorder = new Box(width = w + 2 * border, height = border, depth = border)
    topBorder.translateX = w / 2
    topBorder.translateY = h + border / 2

    leftBorder.translateX = -border / 2
    leftBorder.translateY = h / 2
    rightBorder.translateX = w + border / 2
    rightBorder.translateY = h / 2

    transforms += new Translate(
      -Display.width / 2.0 + grid.ui.loc.x,
      0.0,
      -Display.height / 2.0 + grid.ui.loc.y
    )
    transforms += new Scale(grid.hints.scaleX, grid.hints.scaleY)
    transforms += new Translate(-w / 2.0, 0.0, 0.0)

    children ++= List(leftBorder, plot, topBorder, rightBorder)
    Textures(METAL, leftBorder)
    Textures(METAL, rightBorder)
    Textures(METAL, topBorder)
  }
  protected class Battery3DFX(val ys: Y) extends Group with Tickable3DFX with Logging {
    import BatterySnapshot.r
    def node = this
    val top = new Cylinder(radius = r, height = 5.0)
    val glass = new Cylinder(radius = r, height = 60.0) with Phong
    val bar = new Cylinder(radius = glass.radius.value - 0.5, height = glass.height.value) with Phong
    val base = new Cylinder(radius = r, height = 5.0)
    glass.phong.specularColor = Color.web("#ccc")
    glass.phong.specularPower = 40.0
    children.addAll(bar, glass, top, base)
    userData = ys
    glass.userData = ys // for pick
    bar.userData = ys // for pick

    Textures(METAL, top)
    Textures(METAL, base)

    glass.rotate = 180.0
    glass.rotationAxis = Rotate.XAxis
    glass.phong.diffuseMap.value = BatterySnapshot.snapshot(ys)
    val yScale = ys.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
    val posEdgeCol = yColorMap(ys)
    val negEdgeCol = posEdgeCol.interpolate(Color.Black, 0.6)
    val posCol = posEdgeCol.opacity(0.25)
    val negCol = negEdgeCol.opacity(0.25)

    bar.phong.specularPower = 80.0
    bar.phong.specularColor = posEdgeCol.interpolate(Color.White, 0.3)

    base.translateY <== base.height / 2.0
    glass.translateY <== base.height + glass.height / 2.0
    bar.translateY <== base.height + (bar.scaleY * bar.height / 2.0)
    top.translateY <== top.height / 2.0 + glass.height + base.height
    translateX = -Display.width / 2.0 + ys.ui.loc.x
    translateZ = -Display.height / 2.0 + ys.ui.loc.y - 0.75 * r
    def tick(te: Tick): Unit = {
      val act = ys.act
      val mag = Math.abs(act)
      val bscale = mag / yScale
      val barJ = bar.delegate
      val thisJ = this.delegate
      if (bscale > 1.0) {
        barJ.setScaleY(1.0)
        var bscale2 = 1.0 + 0.5 * (bscale - 1.0)
        if (bscale2 > 2.0)
          bscale2 = 2.0
        thisJ.setScaleZ(bscale2)
        thisJ.setScaleX(bscale2)
      } else {
        barJ.setScaleY(bscale)
        thisJ.setScaleZ(1.0)
        thisJ.setScaleX(1.0)
      }
      var edgeCol = posEdgeCol
      val bcol =
        if (act > 0.0)
          posCol
        else {
          edgeCol = negEdgeCol
          negCol
        }
      bar.phong.delegate.setDiffuseColor(bcol)
      if (mag > 0.01)
        renderEdges(te, ys, mag, 2.0 * r, edgeCol, gc)
    }
  }
  object BatterySnapshot {
    import memnets.utils._
    val oneDigit: Boolean = true
    val r = 18.0
    val font = Font(family = "Tahoma", size = 15)
    val root = new Group(FXMLLoader.load("battery3d.fxml".asURL))
    val text = root.findTextById("text")
    val formatter: FastFormatter = if (oneDigit) OneDigit else TwoDigits
    // need scene for css + layout to work
    val tempScene = new Scene(
      width = 200,
      height = 200,
      depthBuffer = true,
      antiAliasing = SceneAntialiasing.Balanced
    ) // magic # ???
    tempScene.content = root
    val params = new SnapshotParameters()
    params.fill = Color.Transparent
    // scale up gives high quality render
    root.scaleX = 2.0
    root.scaleY = 2.0
    val rootScale = 0.5
    val fill = new Rectangle(root.findById("fill"))
    fill.visible = false
    var isInit = false

    def init(): Unit = {
      if (!isInit) {
        // hack : w/o "warm-up" 1st guy has bad bounds
        root.snapshot(params, null)
        isInit = true
      }
    }
    def snapshot(y: Y): Image = {
      val txt = y.description
      text.setText(txt)
      if (txt.length > 2) {
        text.setRotate(0.0)
        text.setTranslateY(0.0)
      } else {
        text.setRotate(90.0)
        text.setTranslateY(1.5)
      }
      root.layout()
      val bnds = root.boundsInParent.value
      val w = bnds.getWidth.asInstanceOf[Int]
      val h = bnds.getHeight.asInstanceOf[Int]
      root.snapshot(params, new WritableImage(w, h))
    }
  }
}
