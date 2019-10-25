package memnets.lwjgl.swt.utils

import org.eclipse.swt.SWT
import org.eclipse.swt.events._
import org.eclipse.swt.graphics.Rectangle
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.opengl._
import org.eclipse.swt.widgets._
import org.joml.Vector3f
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._

abstract class AbstractGLApp(
    width: Int = 1080,
    height: Int = 720,
    minClientWidth: Int = 640,
    minClientHeight: Int = 480)
    extends Runnable
    with App {
  val display = new Display()
  val shell = new Shell(display)
  shell.setLayout(new FillLayout())
  shell.addKeyListener(new KeyAdapter() {
    override def keyPressed(e: KeyEvent): Unit = {
      if (e.stateMask == SWT.ALT && (e.keyCode == SWT.KEYPAD_CR || e.keyCode == SWT.CR))
        shell.setFullScreen(!shell.getFullScreen)
    }
  })
  val dw = shell.getSize().x - shell.getClientArea().width
  val dh = shell.getSize().y - shell.getClientArea().height
  shell.setMinimumSize(minClientWidth + dw, minClientHeight + dh)
  val data = new GLData()
  data.doubleBuffer = true
  val canvas = new GLCanvas(shell, SWT.NO_BACKGROUND | SWT.NO_REDRAW_RESIZE, data)
  canvas.setCurrent()
  val rect = new Rectangle(0, 0, 0, 0)
  canvas.addListener(SWT.Resize, { e =>
    val bounds = canvas.getBounds()
    rect.width = bounds.width
    rect.height = bounds.height
    glViewport(0, 0, rect.width, rect.height)
  })
  shell.addListener(SWT.Traverse, { e =>
    e.detail match {
      case SWT.TRAVERSE_ESCAPE =>
        shell.close()
        e.detail = SWT.TRAVERSE_NONE;
        e.doit = false;
      case default =>
    }
  })
  GL.createCapabilities()
  shell.setSize(width, height)
  shell.open()

  protected def onDoubleClick(): Unit = {}
  var dragging = false
  var startX, startY = 0
  canvas.addGestureListener { ge =>
    //   ge.magnification
//    println("gesture: "+ge)
  }
  canvas.addMouseListener(new MouseListener {
    def mouseDoubleClick(mouseEvent: MouseEvent): Unit = { onDoubleClick() }
    def mouseDown(mouseEvent: MouseEvent): Unit = { println("mouseDown") }
    def mouseUp(mouseEvent: MouseEvent): Unit = { dragging = false }
  })
  canvas.addDragDetectListener { dde =>
    dragging = true
    startX = dde.x
    startY = dde.y
    fixedRotate = new Vector3f(rotate)
  }
  canvas.addMouseMoveListener { me =>
    if (dragging) {
      val delX: Float = (startX - me.x) / 2.0f
      val delY: Float = (startY - me.y) / 2.0f
      rotate.z = fixedRotate.z + delX
      rotate.x = fixedRotate.x - delY
      cameraDirty = true
    }
  }
  canvas.addMouseWheelListener { me =>
    println("scroll: " + me)
    pos.z += -40.0f * me.count
    cameraDirty = true
  }
  /*
  canvas.addMouseTrackListener(new MouseTrackListener {
    def mouseEnter(mouseEvent: MouseEvent): Unit = ???
    def mouseExit(mouseEvent: MouseEvent): Unit = ???
    def mouseHover(mouseEvent: MouseEvent): Unit = ???
  })
   */
  import org.joml.Vector3f
  protected val pos = new Vector3f(0.0f, 0.0f, 3150.0f)
  protected val rotate = new Vector3f(-45.0f, 0.0f, 0.0f)
  protected var fixedRotate = new Vector3f(0.0f, 0.0f, 0.0f)
  protected var cameraDirty = true

  def run(): Unit = {
    if (!canvas.isDisposed) {
      canvas.setCurrent()
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      if (cameraDirty) {
        val aspect: Float = rect.width / rect.height
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        glFrustum(-1.0f, 1.0f, -aspect, aspect, 5.0f, 10000.0f)
        glTranslatef(-pos.x, -pos.y, -pos.z)
        glRotatef(rotate.x, 1.0f, 0.0f, 0.0f)
        glRotatef(rotate.y, 0.0f, 1.0f, 0.0f)
        glRotatef(rotate.z, 0.0f, 0.0f, 1.0f)
        cameraDirty = false
      }

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      renderGL()

      canvas.swapBuffers()
      display.asyncExec(this)
    }
  }
  def runUntilClosed(): Unit = {
    initGL()
    initLighting()
    initApp()
    display.asyncExec(this)
    while (!shell.isDisposed) {
      if (!display.readAndDispatch)
        display.sleep
    }
    display.dispose
  }
  def initApp(): Unit
  def initLighting(): Unit = {}
  def initGL(): Unit = {
    import org.lwjgl.opengl.GL11

    glEnable(GL_CULL_FACE)
    glCullFace(GL_BACK)
    glEnable(GL_DEPTH_TEST)

    glEnable(GL_NORMALIZE)
    glEnable(GL_TEXTURE)
    glEnable(GL_TEXTURE_2D)

    // set background color
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClearDepth(1)

    glEnable(GL11.GL_BLEND)
    glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
    glViewport(0, 0, rect.width, rect.height)
  }
  def renderGL(): Unit
}
