package memnets

import java.awt.Color

import com.typesafe.scalalogging.StrictLogging
import memnets.utils.{BeanSupport, DirtySupport}
import org.lwjgl.opengl.GL11._

import scala.collection.mutable

package object lwjgl extends StrictLogging {
  type Col = java.awt.Color
  private val _fxCol2Array = mutable.AnyRefMap[Col, Array[Float]]()
  private val _nullColorArray = Array(
    1.0f,
    1.0f,
    1.0f,
    1.0f
  )
  implicit class ColorExt(val c: Col) extends AnyVal {
    def toArray(): Array[Float] = {
      if (c != null) {
        var opt = _fxCol2Array.get(c)
        if (opt.isEmpty) {
          //logger.debug("adding to cache: " + c)
          val array = c.getRGBComponents(null)
          _fxCol2Array(c) = array
          opt = Some(array)
        }
        opt.get
      } else
        _nullColorArray
    }
  }
  trait PhongGL {
    val phong = new PhongMaterialGL
  }
  class PhongMaterialGL extends BeanSupport with DirtySupport {
    var _diffuse: Col = Color.GRAY
    var _specular: Col = Color.LIGHT_GRAY
    var _specularPower = 60.0f

    def diffuse = _diffuse
    def diffuse_=(value: Col): Unit = {
      _diffuse = value
      dirty = true
    }
    def specular = _specular
    def specular_=(value: Col): Unit = {
      _specular = value
      dirty = true
    }
    def specularPower = _specularPower
    def specularPower_=(value: Float): Unit = {
      _specularPower = value
      dirty = true
    }
    override def dirty_=(value: Boolean): Unit = {
      super.dirty_=(value)
      _dirty = false
    }
    def applyGL11(): Unit = {
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, _diffuse.toArray)
      glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, _specular.toArray)
      glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, BLACK_GL)
      glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, _specularPower)
    }
  }

  val BLACK_GL = Color.BLACK.toArray
  implicit def tgl2Option[T <: TickableGL](fx: T): Option[TickableGL] = Some(fx)
}
