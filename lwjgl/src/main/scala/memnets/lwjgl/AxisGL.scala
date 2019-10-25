package memnets.lwjgl

import java.awt.Color

import memnets.lwjgl.util.AxisType
import memnets.lwjgl.util.GLUtils.normal3f
import org.lwjgl.opengl.GL11._

class AxisGL(val axisType: AxisType, val axisLen: Float = 450.0f) {
  var posCol: Col = Color.GRAY
  var negCol: Col = Color.DARK_GRAY
  var visible = true
  def color(col: Col): Unit = {
    posCol = col
    negCol = col.darker().darker()
  }
  def tick(): Unit = {
    if (visible) {
      glPushMatrix()
      glLoadIdentity()
      if (axisType == AxisType.YAxis)
        glRotatef(90.0f, 0.0f, 0.0f, 1.0f)
      else if (axisType == AxisType.ZAxis)
        glRotatef(-90.0f, 0.0f, 1.0f, 0.0f)

      glBegin(GL_LINES)
      glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, Color.WHITE.toArray())
      glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, BLACK_GL)
      glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 30.0f)

      // setting one normal here for all axes
      normal3f(0.0f, 0.0f, 1.0f)
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, posCol.toArray)
      glVertex3f(0.0f, 0.0f, 0.0f)
      glVertex3f(axisLen, 0.0f, 0.0f)
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, negCol.toArray)
      glVertex3f(0.0f, 0.0f, 0.0f)
      glVertex3f(-axisLen, 0.0f, 0.0f)
      glEnd()
      glPopMatrix()
    }
  }
}
