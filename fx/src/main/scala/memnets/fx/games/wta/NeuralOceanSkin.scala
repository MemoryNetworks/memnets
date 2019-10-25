package memnets.fx.games.wta

import memnets.fx._
import memnets.fx.fx3d.Textures._
import memnets.fx.fx3d._
import memnets.model._
import memnets.models.neuro.swta.SoftWTA
import org.fxyz3d.shapes._
import scalafx.scene.Node
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout._
import scalafx.scene.paint._
import scalafx.scene.shape._
import scalafx.scene.transform.Rotate

class NeuralOceanSkin(val board: Board, val ctrl: SoftWTA) extends SkinFX with Logging {
  name = "Ocean"
  chartOn = false
  var sc3d: Scene3DFX = _
  val tileSize = 1024.0
  val dim = board.rows
  val boardSize = tileSize / 2.0
  val fxDim: Double = boardSize / dim
  val fxYScale = fxDim / 6.0
  val defaultThresh = board.defaultThresh
  override def zoomDefault(system: DynamicSystem): Double = 0.9
  override def createSystem(system: DynamicSystem): Iterable[TickableFX] = {
    val container = new ContainerFX(sc3d)
    if (!sc3d.showGlass) container.frame.hideGlass
    if (sc3d.showBorder) container.frame.showBorder
    List(container)
  }
  override def init(system: DynamicSystem): Unit = {
    super.init(system)
    sc3d = new Scene3DFX(
      loc = Loc().down(65),
      w = 700,
      h = 500,
      zoom = 2.5,
      tiltDown = 50.0,
      showBorder = true,
      showGlass = false
    )
    val dome = sc3d.addSkyDome(radius = 3 * tileSize / 2, yOff = -1.0)
    // set these after SkyDome as he may change
    sc3d.sun.translateY = 600
    sc3d.sun.translateZ = 900
    val oceanTile = Plane3DFX(texture = OCEAN, w = tileSize, h = 256, yoffset = 1.0)
    sc3d.addContent(oceanTile)
    val worldTile = Plane3DFX(texture = OCEAN, w = 2 * dome.shape.radius.value.toInt, h = 256, yoffset = -1.0)
    sc3d.addContent(worldTile)
    sc3d.addOceanLight(oceanTile, worldTile)

    // make border blocks
    val bDim = boardSize / 2 + fxDim / 2
    object top extends Box(boardSize + 2 * fxDim, fxDim / 4.0, fxDim) with Phong
    object bottom extends Box(boardSize + 2 * fxDim, fxDim / 4.0, fxDim) with Phong
    object left extends Box(fxDim, fxDim / 4.0, boardSize) with Phong
    object right extends Box(fxDim, fxDim / 4.0, boardSize) with Phong
    top.translateZ.value = -bDim
    bottom.translateZ.value = bDim
    left.translateX.value = -bDim
    right.translateX.value = bDim
    for (b <- List(top, bottom, left, right)) {
      Textures(Textures.METAL, b, share = true)
      sc3d.addContent(b)
    }
    import board._
    class Panel(ctrl: SoftWTA) extends TickableFX {
      val diam = 80.0
      val w = ctrl.length * diam
      object canvas extends Canvas {
        width = w
        height = 120.0
      }
      canvas.opacity = 0.85
      canvas.prefWidth(w)
      canvas.prefHeight(120.0)
      val node: Option[Node] = new BorderPane { center = canvas }
      val pg2 = canvas.graphicsContext2D
      def tick(te: Tick): Unit = {
        val h = canvas.height.value
        pg2.clearRect(0.0, 0.0, w, h)
        for ((c, i) <- ctrl.zipWithIndex) {
          pg2.fill = yColorMap(c)
          val scale = Math.max(0.1, c.act / 6.0)
          val diam2 = diam * scale
          pg2.fillOval(x = diam / 2.0 + i * diam - diam2 / 2.0, y = h / 2.0 - diam2 / 2.0, w = diam2, h = diam2)
        }
      }
    }
    customPanel = Option(new Panel(ctrl))
    val start = -grid.rows * fxDim / 2 + fxDim
    for ((r, c, y) <- grid.units) {
      val m = memGrid(r, c)
      val loc = Loc(start + c * fxDim, start + r * fxDim)
      y.ui.loc = loc
      m.ui.loc = loc
      val yFX = GridUnitFX(y, h = 4.0 * fxDim, texture = OCEAN_GLASS, shareTexture = true)
      val mFX = MemUnitFX(m)
      sc3d.add(yFX)
      sc3d.add(mFX)
      for (item <- board(r, c)) {
        item match {
          case w: Wall =>
            val y = w.thres
            y.ui.loc = loc
            sc3d.add(new WallFX(y))
          case c: Combo         => sc3d.add(ComboFX(y, c))
          case un: Unknown      => sc3d.add(UnknownFX(un))
          case wh: WormHole     => sc3d.add(WormHoleFX(wh))
          case cv: ConveyorBase => sc3d.add(ConveyorFX(y, cv))
          case default          =>
        }
      }
    }
    // nasty hack
    import scala.collection.JavaConverters._
    sc3d.sun.scope ++= sc3d._worldGroup.children.asScala
    sc3d.sun.scope -= dome
    var maxY = grid(rows / 2, cols / 2)
    sc3d.autoCameraOn.value = true
    sc3d.autoCamera = { (te, scene) =>
      if (te.quarter) {
        var maxAct = 0.0
        for {
          rows <- grid.rowData
          u <- rows if u.act > maxAct
        } {
          maxY = u
          maxAct = maxY.act
        }
      }
      // rolling ocean animation
      oceanTile.delegate.setTranslateY(
        -oceanTile.h / 2.0 + oceanTile.yoffset + 0.5 * Math.cos(2.0 * Math.PI * te.t / 200.0))
      def interp(curr: Double, next: Double) = curr + (next - curr) / 20.0
      val pos = scene.cameraPos
      scene.moveCamera(
        interp(pos.x, -maxY.ui.loc.x + fxDim / 2.0),
        interp(pos.y, fxYScale * -maxY.act),
        interp(pos.z, -maxY.ui.loc.y)
      )
    }
    sc3d.init()
  }

  override def create(elem: Element): Option[TickableFX] = None
  def createY(y: Y): Option[TickableFX] = None
  override def initCanvas(elemPane: Pane, canvas: Canvas): List[Node] = {
    canvas.visible = false
    canvas.effect = null
    List(elemPane)
  }
  override def clearCanvas(canvas: Canvas) = {}
  override def toString: String = s"OceanSkin[]"

  sealed case class GridUnitFX(
      y: Y,
      texture: String = "",
      shareTexture: Boolean = false,
      yOff: Double = 0.0,
      yScale: Double = fxYScale,
      h: Double = fxDim)
      extends Tickable3DFX {
    object fx extends Box(fxDim, h, fxDim) with Phong {
      pickOnBounds = true
      Textures(texture, this, share = shareTexture)
      if (!shareTexture) phong.diffuseColor = yColorMap(y)
    }
    def node = fx
    def tick(te: Tick): Unit = {
      val act = y.act
      if (act > 0.0) {
        fx.translateY = -h / 2.0 + yOff + act * yScale
        fx.visible = true
      } else
        fx.visible = false
    }
    fx.translateX.value = y.ui.loc.x - fxDim / 2
    fx.translateZ.value = y.ui.loc.y - fxDim / 2
  }
  sealed case class MemUnitFX(y: Y, yScale: Double = fxYScale) extends Tickable3DFX {
    object fx extends Sphere(fxDim / 2, divisions = 16) with Phong {
      pickOnBounds = true
      Textures(Textures.CLOUD, this, share = false)
      phong.specularPower = 100.0
      phong.specularColor = yColorMap(y).interpolate(Color.Black, 0.4)
      phong.diffuseColor = yColorMap(y).opacity(0.8)
    }
    def node = fx
    def tick(te: Tick): Unit = {
      if (te.even) {
        val act = y.act
        if (act > 0.0) {
          fx.translateY = -fxDim / 2 + 3.85 * fxDim + y.act * 10.0 * yScale
          fx.visible.value = true
          if (te.quarter) {
            fx.scaleX = 1.0 + act
            fx.scaleZ = 1.0 + act
          }
        } else
          fx.visible.value = false
      }
    }
    fx.visible.value = false
    fx.translateX.value = y.ui.loc.x - fxDim / 2.0
    fx.translateZ.value = y.ui.loc.y - fxDim / 2.0
  }

  sealed case class ComboFX(y: Y, c: Combo, yScale: Double = fxYScale) extends Tickable3DFX {
    val mem = c(y)
    object fx extends Sphere(fxDim / 2, divisions = 16) with Phong {
      pickOnBounds = true
      phong.specularPower = 160.0
      phong.specularColor = Color.web("#222")
      phong.diffuseColor = yColorMap(c.ctl).opacity(0.8)
    }
    def node = fx
    def tick(te: Tick): Unit = {
      val act = c.prior.act
      fx.visible.value = act > 0.1
    }
    fx.translateX.value = y.ui.loc.x - fxDim / 2.0
    fx.translateZ.value = y.ui.loc.y - fxDim / 2.0
  }
  sealed case class UnknownFX(unk: Unknown, yScale: Double = fxYScale) extends Tickable3DFX {
    val y = unk.unit
    object fx extends Sphere(fxDim / 2, divisions = 16) with Phong {
      pickOnBounds = true
      phong.specularPower = 160.0
      phong.specularColor = Color.web("#222")
      phong.diffuseColor = yColorMap(y)
    }
    def node = fx
    def tick(te: Tick): Unit = {}
    fx.translateX.value = y.ui.loc.x - fxDim / 2.0
    fx.translateZ.value = y.ui.loc.y - fxDim / 2.0
  }
  sealed case class WormHoleFX(wh: WormHole, yScale: Double = fxYScale) extends Tickable3DFX {
    val y = wh.src
    val fx = new Torus(fxDim / 2, fxDim / 4) {
      val phong = new PhongMaterial {}
      phong.specularPower = 160.0
      phong.specularColor = Color.web("#222")
      phong.diffuseColor = yColorMap(y)
      setMaterial(phong)
    }.extractShape()
    fx.setRotationAxis(Rotate.XAxis)
    fx.setRotate(90)

    def node = fx
    def tick(te: Tick): Unit = {}
    fx.setTranslateX(y.ui.loc.x - fxDim / 2.0)
    fx.setTranslateZ(y.ui.loc.y - fxDim / 2.0)
  }
  sealed case class ConveyorFX(y: Y, belt: ConveyorBase, yScale: Double = fxYScale) extends Tickable3DFX {
    val fx = new Cone(32, fxDim / 2.0, fxDim) {
      val phong = new PhongMaterial {}
      phong.specularPower = 160.0
      phong.specularColor = Color.web("#922")
      phong.diffuseColor = yColorMap(y)
      setMaterial(phong)
    }.extractShape()
    fx.setRotationAxis(Rotate.ZAxis)
    fx.setRotate(90)
    def node = fx
    def tick(te: Tick): Unit = {}
    fx.setTranslateX(y.ui.loc.x - fxDim / 2.0)
    fx.setTranslateZ(y.ui.loc.y - fxDim / 2.0)
    fx.setTranslateY(-fxDim / 2.0)
  }
  class WallFX(y: Y) extends GridUnitFX(y, texture = WOOD, shareTexture = true, yScale = fxDim / defaultThresh) {
    override def tick(te: Tick): Unit = {
      if (te.quarter) {
        val act = y.act
        if (act > 0.001) {
          fx.translateY = -1.0 * h + y.act * yScale
          fx.visible.value = true
        } else
          fx.visible.value = false
      }
    }
  }
}
