package memnets.lwjgl.swt

import java.awt.Color

import memnets.awt._
import memnets.core._
import memnets.lwjgl.{ModelGL, _}
import memnets.lwjgl.swt.utils.AbstractGLApp
import memnets.model._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._

abstract class AppBaseGL(val sleepTime: Int = 15) extends AbstractGLApp with AppUI with Logging {
  protected val _config = ModelConfig()
  val modelGL = new ModelGL
  val engine = new Engine(modelUI = modelGL, appUI = this)
  val sideBar = new SideBarGL(display, engine)

  def config = _config
  def loadResources(): Unit
  def startUpBuilder: ModelBuilder

  /** allows using only glColor to set material but interferes with full material support */
  def colorMaterial: Boolean = false
  override protected def onDoubleClick(): Unit = { engine.fireReset() }
  override def initApp(): Unit = {
    loadResources()

    // todo: check startup args for HD1080
    // don't change after startup.  just scale...
    Display.resolution = HD720.getResolution

    engine.setLoop(true)
    engine.setBuilder(startUpBuilder)
  }
  override def initGL(): Unit = {
    super.initGL()
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glHint(GL_POINT_SMOOTH, GL_NICEST)
    glEnable(GL_POINT_SMOOTH)
    glEnable(GL_VERTEX_PROGRAM_POINT_SIZE)
  }
  override def initLighting(): Unit = {
    if (colorMaterial) {
      glEnable(GL_COLOR_MATERIAL)
      glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
    } else
      glDisable(GL_COLOR_MATERIAL)

    glShadeModel(GL_SMOOTH)
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)

    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, toGray(0.2).toArray)
    glLightfv(GL_LIGHT0, GL_AMBIENT, Color.BLACK.toArray)
    glLightfv(GL_LIGHT0, GL_DIFFUSE, Color.WHITE.toArray)
    glLightfv(GL_LIGHT0, GL_SPECULAR, Color.WHITE.toArray)
    glLightfv(GL_LIGHT0, GL_POSITION, Array(0.0f, -500.0f, 500.0f, 0.0f))
  }
  def renderGL(): Unit = {
    // filters calls to render/engine until signals ready
    engine.tick(System.nanoTime())
    // todo : nasty hack
    Thread.sleep(sleepTime)
  }
  def process(ee: EngineEvent): Unit = ee match {
    case BuilderEvent(builder) =>
      sideBar.selectBuilder(builder)
    case TrialEvent(trial) =>
      sideBar.selectTrial(trial)
    case ErrorEvent(msg, ex) =>
      logger.error("unknown error: " + msg)
      throw ex
    case default =>
  }
  runUntilClosed()
}
