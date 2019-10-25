package memnets

import java.awt._

import memnets.model.Loc
import memnets.ui.{ColorMap, YColorMap}

package object awt {
  type Col = java.awt.Color
  type YCol = YColorMap[Col]
  type ColMap = ColorMap[Col]
  val INH_COLOR = Color.LIGHT_GRAY
  val INH_EDGE_COLOR = INH_COLOR
  val FUNC_COLOR = Color.LIGHT_GRAY
  val ACC_COLOR = Color.GREEN
  val USER_COLOR = Color.WHITE

  val HEAT_COLORMAP: ColMap = HeatMapAwt()

  implicit def tswg2Option[T <: JTickable](fx: T): Option[JTickable] = Some(fx)

  implicit class Graphics2SExt(val g2: Graphics2D) extends AnyVal {
    def clearP(w: Int, h: Int): Unit = {
      import java.awt.AlphaComposite
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR))
      g2.fillRect(0, 0, w, h)
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER))
    }
  }
  implicit class LocExt(val loc: Loc) extends AnyVal {
    def toPoint: java.awt.Point = new java.awt.Point(loc.x.toInt, loc.y.toInt)
  }
  def toGray(value: Double): Color = {
    val c: Int = (255 * value).asInstanceOf[Int]
    new Color(c, c, c)
  }

  /** NOTE: potential issue w/ thread safety  */
  private val _tempComps = Array.ofDim[Float](4)
  implicit class ColorAwtExt(val c: Color) extends AnyVal {
    implicit def d2f(d: Double): Float = d.asInstanceOf[Float]
    def trans(alpha: Double = 0.2): Color = {
      val comps = c.getRGBComponents(_tempComps)
      new Color(comps(0), comps(1), comps(2), comps(3) * alpha)
    }
    def opac(o: scala.Double): Color = trans(o)
    def getOpacity = c.getAlpha

    def hue(shift: scala.Double): Color = {
      val comps = Color.RGBtoHSB(c.getRed, c.getGreen, c.getBlue, _tempComps)
      new Color(Color.HSBtoRGB(comps(0) * (1.0 - shift), comps(1), comps(2)))
    }

  }

}
