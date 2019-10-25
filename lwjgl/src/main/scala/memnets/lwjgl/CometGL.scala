package memnets.lwjgl

import memnets.lwjgl.util.GLUtils._
import memnets.model.Logging
import org.lwjgl.opengl.GL11._

class CometGL(
    val scale: Float = 1.0f,
    val window: Int = 200,
    val temporalZ: Float = 1.0f
) extends Logging {
  val radiusScale: Float = 8.0f / window.toFloat
  val spheres = Array.tabulate(window) { i =>
    createBall(i)
  }
  def ballRadius(i: Int): Float = 1.0f + (window - i) * radiusScale
  def createBall(i: Int): SphereGL = {
    val s = new SphereGL()
    s.visible = false
    s.radius = ballRadius(i)
    s.phong.specularPower = 10.0f
    s
  }
  var temporal = false
  val pts = Array.fill(window) { Pt3Df() }
  var index = 0
  def reset(): Unit = {
    index = 0
    for (s <- spheres) s.visible = false
    for (s <- pts) s.clear()
  }
  def tickHelper(i: Int, pt: Pt3Df, ball: SphereGL): Unit = {}
  def tick(x: Double, y: Double, z: Double): Unit = {
    val pt = pts(index)
    pt.x = x.asInstanceOf[Float]
    pt.y = y.asInstanceOf[Float]
    pt.z = z.asInstanceOf[Float]
    if (index < window) spheres(index).visible = true
    // transfer mesh pts to spheres
    var i = 0
    while (i < window) {
      var j = index - i
      if (j < 0) j = window + j
      val pt = pts(j)
      val sp = spheres(i)
      sp.loc.x = scale * pt.x
      sp.loc.y = scale * pt.y
      sp.loc.z = if (temporal) -temporalZ * i else scale * pt.z
      tickHelper(j, pt, spheres(i))
      i += 1
    }
    index = index + 1
    if (index >= spheres.size) index = 0

    glPushMatrix()
    glLoadIdentity()
    normal3f(1.0f, -1.0f, 1.0f)
    val len = spheres.length
    i = 0
    while (i < len) {
      val sphere = spheres(i)
      if (sphere.visible) {
        sphere.phong.applyGL11()
        glPointSize(sphere.radius)
        glBegin(GL_POINTS)
        glVertex3d(sphere.loc.x, sphere.loc.y, sphere.loc.z)
        glEnd()
      }
      i += 1
    }
    glPopMatrix()
  }
}
