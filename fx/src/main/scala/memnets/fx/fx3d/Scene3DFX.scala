package memnets.fx.fx3d

import memnets.fx._
import memnets.fx.utils.SubSceneP
import memnets.model._
import memnets.ui._
import scalafx.Includes._
import scalafx.beans.property.BooleanProperty
import scalafx.scene._
import scalafx.scene.control._
import scalafx.scene.paint.Color
import scalafx.scene.shape._
import scalafx.scene.transform._

import scala.collection.mutable._

object Scene3DFX extends Logging {
  private[memnets] var _handler: Element => Unit = elem => {
    logger.warn("no handler for element event!!!")
  }
  private[memnets] def firePickEvent(elem: Element) = _handler(elem)

  val DEFAULT_NAME = "Scene3DFX"
}

class Scene3DFX(
    override var loc: Loc = Loc().left(250),
    val w: Double = 600,
    val h: Double = 600,
    var zoom: Double = 1.0,
    var rotate: Double = 0.0,
    var tiltDown: Double = 25.0,
    var cameraZ: Double = -1400.0,
    val showAxes: Boolean = false,
    val showGlass: Boolean = false,
    val showBorder: Boolean = true,
    val showFrame: Boolean = true,
    override val minusHeader: Boolean = true
) extends ElementBase
    with Scene3D
    with FullSceneFX
    with FrameFXStyle
    with Logging {

  name = Scene3DFX.DEFAULT_NAME // optional way to find
  val autoCameraOn = BooleanProperty(false)
  var autoCamera: AutoCamera = NullAutoCamera
  val subScene3d = new SubSceneP with FrameFXStyle {
    var paneStyle: Option[String] = None
    userData = this
  }
  val ambientLight = new AmbientLight { color = Color.web("#ccc") }
  val sun = new PointLight {
    color = Color.web("#ccc")
    translateX = -500
    translateY = 300
    translateZ = 0
  }
  var originLight: Option[PointLight] = None

  protected val tickables = ArrayBuffer[Tickable3DFX]()
  private var _cameraInit = false
  // Put shapes in a groups so they can be rotated together
  private[memnets] val _worldGroup = new Group()
  private val _hackGroup = new Group()

  val mainGroup = new Group {
    managed = false
    depthTest = DepthTest.Enable
    children.addAll(ambientLight, sun, _worldGroup, _hackGroup)
  }
  subScene3d.content.add(mainGroup)
  subScene3d.picked ==> {
    _.userData match {
      case elem: Element =>
        Scene3DFX.firePickEvent(elem)
      case default =>
    }
  }
  subScene3d.userData = this
  subScene3d.cameraZ = cameraZ
  if (showAxes) addContent(new Axes3DFX())
  subScene3d.onContextMenuRequested = me => {
    logger.trace("fullScreenFX request")
    val items = ListBuffer[MenuItem]()
    items += new RadioMenuItem("Full Window") {
      selected <==> subScene3d.fullScreen
      onAction = e => { toggleFullScene() }
    }

    val rmi = new RadioMenuItem("Auto-Camera") {
      selected <==> autoCameraOn
    }
    rmi.disable = autoCamera == NullAutoCamera
    items += rmi
    val menu = new ContextMenu(items.toSeq: _*) {
      autoHide = true
      show(subScene3d, me.getScreenX, me.getScreenY)
    }
    // these aren't picked up in model.css if from subscene.
    // could use shared css file on subscene...
    menu.style = """
                   | -fx-font-family: "Tahoma";
                   | -fx-base: #222;
                   | -fx-background-color: rgba(0,0,0,0.5);
                   | -fx-fill: derive(-fx-base,+0%);
                   | -fx-font-smoothing-type: lcd;
                   |      """.stripMargin
    me.consume()
  }
  subScene3d.onMouseClicked = me => {
    me.getClickCount match {
      case 2 =>
        resetCamera()
      case default =>
    }
    me.consume
  }

  def camera = subScene3d.camera.value
  def find(elem: Element): Option[Tickable3DFX] = tickables.find(_.element == elem)
  def node = subScene3d
  def paneStyle: Option[String] = subScene3d.paneStyle
  def remove(t: Tickable3DFX): Unit = {
    logger.trace(s"remove : $t")
    tickables -= t
    for (fx <- t.node) {
      var found = _worldGroup.children.remove(fx)
      if (!found)
        found = subScene3d.content.remove(fx)
      if (!found) logger.warn("not found: " + t)
    }
    t.destroy()
  }
  def removeE(elem: Element): Unit = {
    for (t <- find(elem))
      remove(t)
  }
  def +=(tfxs: Tickable3DFX*): Unit = {
    for (tfx <- tfxs) {
      for (fx <- tfx.node)
        _worldGroup.children.add(fx)
      tickables += tfx
    }
  }
  def add(tfx: Tickable3DFX): Unit = { +=(tfx) }
  def addPrimary(tfx: Tickable3DFX): Unit = {
    for (fx <- tfx.node)
      subScene3d.content.add(fx)

    tickables += tfx
  }
  def addContent(node: JNode*): Unit = {
    for (n <- node)
      _worldGroup.children.add(n)
  }
  def addSkyDome(
      radius: Double = 1024.0,
      divisions: Int = 64,
      yOff: Double = 0.0,
      doFarClip: Boolean = true,
      texture: String = Textures.SKY,
      yRotate: Double = -65.0): SelfLit3DFX[Sphere] = {

    val dome = new Sphere(radius = radius, divisions = divisions)
    dome.transforms = List(
      new Translate(x = 0.0, y = yOff, z = 0.0),
      new Rotate(180.0, Rotate.ZAxis),
      new Rotate(yRotate, Rotate.YAxis))
    dome.cullFace = CullFace.Front
    ambientLight.lightOn = true
    ambientLight.color = Color.web("#aaa")
    sun.color = Color.White.interpolate(Color.Orange, 0.4)
    sun.translateX = 0.0
    sun.translateZ = -radius
    sun.translateY = 0.7 * radius
    sun.lightOn = true
    Textures(texture, dome)
    if (doFarClip) camera.farClip = 6 * radius
    val cont = SelfLit3DFX(dome)
    addContent(cont)
    cont
  }
  def addSkyBox(
      size: Double = 2048.0,
      doFarClip: Boolean = true,
      texture: String = Textures.SKY,
      ambient: Color = Color.web("#888")): Box = {
    val box = new Box(size, size, size)
    box.cullFace = CullFace.Front
    box.rotate = 180.0
    if (ambient != null) {
      ambientLight.lightOn = true
      ambientLight.color = ambient
    }
    sun.translateX = 0.0
    sun.translateZ = 0.0
    sun.translateY = size / 2.0 // center light

    Textures(texture, box)
    if (doFarClip) camera.farClip = 4 * size
    addContent(box)
    box
  }
  def addLight(light: PointLight): Unit = { mainGroup.children += light }
  def addOriginLight(webColor: String = "#ddd"): PointLight = {
    val lt = new PointLight {
      color = Color.web(webColor)
      translateX = 0
      translateY = 1
      translateZ = 0
      lightOn = true
    }
    originLight = Some(lt)
    addLight(lt)
    lt
  }
  def addOceanLight(nodes: JNode*): PointLight = {
    val oceanLight = new PointLight {
      color = Color.web("#cca37a")
      translateY = 4000.0
      translateZ = -10000.0
      scope ++= nodes
    }
    addLight(oceanLight)
    oceanLight
  }
  def tick(te: Tick): Unit = {
    val len = tickables.length
    var i = 0
    while (i < len) {
      tickables(i).tick(te)
      i += 1
    }
    if (autoCameraOn.value)
      autoCamera.tick(te, this)

    // HACK!!!!!!!
    // if don't have one VISIBLE node being modified each tick
    // 60FPS limit turns OFF when in fullscreen!!!
    // still an issue when window in background
    _hackGroup.translateX = if (te.even) 0.02 else 0.01
  }
  override def reset(): Unit = {
    val toDel = new ListBuffer[Tickable3DFX]()
    for (t <- tickables) {
      if (t.permanent)
        t.reset()
      else
        toDel += t
    }
    for (t <- toDel)
      remove(t)
  }
  override def init(): Unit = {
    if (!subScene3d.delegate.widthProperty.isBound) {
      subScene3d.width = w
      subScene3d.height = h
      val bnds = subScene3d.delegate.getBoundsInLocal
      val bw = bnds.getWidth
      val bh = bnds.getHeight
      logger.trace(s"$name init bounds (w, h) = ($bw, $bh) at $loc")
      subScene3d.relocate(loc.x - bw / 2.0, loc.y - bh / 2.0)
    } else
      logger.trace(s"$name already bound")

    if (null != tickables) // called from super b4 tickables init...
      for (t <- tickables) t.init()

    if (!_cameraInit) {
      resetCamera() // just do first time
      _cameraInit = true
    }
  }
  override def toString: String = s"Scene3D[name= $name, loc= $loc]"

  def fullWidth = subScene3d.width
  def fullHeight = subScene3d.height
  def relocate(x: Double, y: Double): Unit = subScene3d.relocate(x, y)
//  autoCameraOn ++ { sel => if (!sel) resetCamera() }
  // NOTE: can override if too many assumptions here...
  def resetCamera(): Unit = {
    logger.trace("resetCamera")
    subScene3d.tiltXDown(tiltDown) // angleX
    subScene3d.angleY.value = rotate
    subScene3d.angleZ.value = 0.0

    // switch back to default camera
//    subScene3d.camera = subScene3d.defaultCamera
    val cam = camera.delegate
    cam.setTranslateX(0.0)
    cam.setTranslateY(0.0)
    cam.setTranslateZ(subScene3d.cameraZ)
    _worldGroup.translateX = 0.0
    _worldGroup.translateY = 0.0
    _worldGroup.translateZ = 0.0
    subScene3d.zoom.value = zoom
  }
  def moveCamera(x: Double, y: Double): Unit = {
    val grp = mainGroup.delegate
    grp.setTranslateX(x)
    grp.setTranslateY(y)
  }
  def moveCamera(x: Double, y: Double, z: Double): Unit = {
    val grp = mainGroup.delegate
    grp.setTranslateX(x)
    grp.setTranslateY(y)
    grp.setTranslateZ(z)
  }
  object cameraPos extends Pt3D {
    val grp = mainGroup.delegate
    def x = grp.getTranslateX
    def y = grp.getTranslateY
    def z = grp.getTranslateZ
  }
}
