package memnets.fx.utils

import com.typesafe.scalalogging.StrictLogging
import memnets.model._
import scalafx.Includes._
import scalafx.beans.property._
import scalafx.scene._
import scalafx.scene.paint.Color
import scalafx.scene.transform.Rotate

class SubSceneP(w: Int = 300, h: Int = 300)
    extends SubScene(new javafx.scene.Group(), w, h, depthBuffer = true, SceneAntialiasing.Balanced)
    with StrictLogging {

  type JGroup = javafx.scene.Group
  val defaultCamera = new PerspectiveCamera(fixedEyeAtCameraZero = true)
  val fullScreen = BooleanProperty(false)
  val rootP = new Group(this.root.value.asInstanceOf[JGroup]) // get ref from SubScene
  val picked = ObjectProperty[javafx.scene.shape.Shape3D](null)
  val zoom = DoubleProperty(1.0)
  val angleX = DoubleProperty(0)
  val angleY = DoubleProperty(0)
  val angleZ = DoubleProperty(0)
  val xRotate = new Rotate(0, Rotate.XAxis)
  val yRotate = new Rotate(0, Rotate.YAxis)
  val zRotate = new Rotate(0, Rotate.ZAxis)
  var scenex, sceney, scenez = 0.0
  var fixedXAngle, fixedYAngle, fixedZAngle = 0.0
  var zooming = false
  var touchScrolling = false
  var touchMinDelta = false
  var _cameraZ = -600.0

  def cameraZ = _cameraZ
  def cameraZ_=(z: Double): Unit = { _cameraZ = z }
  def tiltXDown(angle: Double): Unit = angleX.value = 180.0 + angle

  xRotate.angle <== angleX
  yRotate.angle <== angleY
  zRotate.angle <== angleZ
  rootP.transforms.addAll(xRotate, yRotate, zRotate)

  rootP.managed = false
  rootP.autoSizeChildren = false
  focusTraversable = true
  //  fill = null  // don't want null or mouse events are finicky
  fill = Color.Transparent
  defaultCamera.nearClip = 0.1
  defaultCamera.farClip = 10000
  defaultCamera.translateZ = _cameraZ // minus Z is up... can't change to +, so just default rotate below
  camera = defaultCamera

  zoom.onChange { (_, _, z) =>
    camera = defaultCamera
    val transZ = defaultCamera.translateZ
    transZ.value = _cameraZ * 1.0 / z.doubleValue
  }
  onZoom = e => {
    zooming = true
    zoom.value = zoom.value * e.getZoomFactor
    e.consume
  }
  onScroll = e => {
    touchMinDelta = Math.abs(e.getDeltaX) > 0.8 || Math.abs(e.getDeltaY) > 0.8

    if (e.isDirect && e.getTouchCount == 3)
      touchScrolling = true

    if (!e.isDirect || touchScrolling) { // touch fires drag and scroll, so filter scroll
      camera = defaultCamera
      if (e.controlDown)
        zoom.value = zoom.value * (defaultCamera.translateZ.value - e.deltaY) / defaultCamera.translateZ.value // zoom
      else
        defaultCamera.translateY.value -= e.deltaY / (if (touchScrolling) 1.0 else 2.0)
    }
    e.consume()
  }
  onMouseReleased = me => {
    zooming = false
    touchScrolling = false
    me.consume()
  }
  onMousePressed = me => {
    scenex = me.getSceneX
    sceney = me.getSceneY
    fixedXAngle = angleX.value
    fixedYAngle = angleY.value
    fixedZAngle = angleZ.value

    val node = me.getPickResult.getIntersectedNode
    logger.trace("node3D = " + node)
    for (fx3d <- node.as[javafx.scene.shape.Shape3D]) {
      logger.debug("picked shape3D: " + fx3d)
      picked.value = fx3d
    }
    me.consume()
  }
  onMouseDragged = me => {
    val goodTouch = if (me.isSynthesized) touchMinDelta else true
    if (!me.isStillSincePress && goodTouch && !zooming && !touchScrolling) {
      camera = defaultCamera
      if (me.isControlDown || me.isSecondaryButtonDown) {
        angleZ.value = fixedZAngle + (sceney - me.getSceneY) / 2.0
        angleY.value = fixedYAngle - (scenex - me.getSceneX) / 2.0
      } else {
        angleX.value = fixedXAngle - (sceney - me.getSceneY) / 2.0
        angleY.value = fixedYAngle - (scenex - me.getSceneX) / 2.0
      }
    }
    me.consume
  }
}
