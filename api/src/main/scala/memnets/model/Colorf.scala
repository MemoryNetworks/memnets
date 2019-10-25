package memnets.model

object Colorf {
  private implicit def d2f(d: Double): Float = d.asInstanceOf[Float]
  def apply(r: Float, g: Float, b: Float, a: Float = 1.0f) = new Colorf(r, g, b, a)
  def apply(r: Double, g: Double, b: Double, a: Double) = new Colorf(r, g, b, a)
  def gray(g: Double) = Colorf(g, g, g, 1.0)
  def hsb(hue: Double, saturation: Double = 1.0, brightness: Double = 1.0, opacity: Double = 1.0): Colorf = {
    val rgb = Colorf.HSBtoRGB(hue, saturation, brightness)
    Colorf(rgb(0), rgb(1), rgb(2), opacity)
  }

  val WHITE = Colorf(1.0, 1.0, 1.0)
  val BLACK = Colorf(0.0, 0.0, 0.0)
  val LIGHTBLACK = Colorf(0.08, 0.08, 0.08) // yeah, i called it that :)

  val GRAY = Colorf(0.74, 0.74, 0.74)
  val LIGHTGRAY = Colorf(0.827451, 0.827451, 0.827451)
  val DARKGRAY = Colorf(0.6627451, 0.6627451, 0.6627451)

  val CRIMSON = Colorf(0.8627451, 0.078431375, 0.23529412)
  val DEEPPINK = Colorf(1.0, 0.078431375, 0.5764706)
  val DODGERBLUE = Colorf(0.11764706, 0.5647059, 1.0)
  val GHOSTWHITE = Colorf(0.972549, 0.972549, 1.0)
  val INDIANRED = Colorf(0.8039216, 0.36078432, 0.36078432)
  val LIMEGREEN = Colorf(0.5647058, 0.9333333, 0.5647058)
  val WHITESMOKE = Colorf(0.9607843, 0.9607843, 0.9607843)
  val YELLOW = Colorf(1.0, 1.0, 0.0)

  val PARAM = Colorf(0.7, 0.7, 0.7)
  val INHIB = PARAM
  val WIN_COLOR = LIMEGREEN.satf(0.8)
  val LOSE_COLOR = CRIMSON.satf(0.8)
  val ACC_COLOR = LIMEGREEN

  /**
   * @return Float[hue, saturation, brightness]
   */
  def RGBtoHSB(r: Float, g: Float, b: Float): Array[Float] = {
    var hue = 0.0f
    var cmax = if (r > g) r else g
    if (b > cmax) cmax = b
    var cmin = if (r < g) r else g
    if (b < cmin) cmin = b
    val brightness = cmax
    val saturation = if (cmax != 0.0f) (cmax - cmin) / cmax else 0.0f
    if (saturation == 0.0f)
      hue = 0.0f
    else {
      val redc = (cmax - r) / (cmax - cmin)
      val greenc = (cmax - g) / (cmax - cmin)
      val bluec = (cmax - b) / (cmax - cmin)
      if (r == cmax)
        hue = bluec - greenc
      else if (g == cmax)
        hue = 2.0f + redc - bluec
      else
        hue = 4.0f + greenc - redc
      hue = hue / 6.0f
      if (hue < 0.0f)
        hue = hue + 1.0f
    }
    Array(hue * 360.0f, saturation, brightness)
  }

  def HSBtoRGB(hue: Float, saturation: Float, brightness: Float): Array[Float] = {
    // normalize the hue
    val normalizedHue = ((hue % 360) + 360) % 360
    val hue2 = normalizedHue / 360
    var r = brightness
    var g = brightness
    var b = brightness
    if (saturation != 0.0f) {
      val h: Float = ((hue2 - Math.floor(hue2)) * 6.0f).asInstanceOf[Float]
      val f: Float = (h - java.lang.Math.floor(h)).asInstanceOf[Float]
      val p = brightness * (1.0f - saturation)
      val q = brightness * (1.0f - saturation * f)
      val t = brightness * (1.0f - (saturation * (1.0f - f)))
      h.asInstanceOf[Int] match {
        case 0 =>
          r = brightness
          g = t
          b = p
        case 1 =>
          r = q
          g = brightness
          b = p
        case 2 =>
          r = p
          g = brightness
          b = t
        case 3 =>
          r = p
          g = q
          b = brightness
        case 4 =>
          r = t
          g = p
          b = brightness
        case 5 =>
          r = brightness
          g = p
          b = q
      }
    }
    Array(r, g, b)
  }

}

case class Colorf(data: Array[Float]) {
  import Colorf.d2f
  require(data.length == 4)
  def this(r: Float, g: Float, b: Float, a: Float = 1.0f) {
    this(Array(r, g, b, a))
  }
  def r = data(0)
  def g = data(1)
  def b = data(2)
  def a = data(3)
  def darkerf(factor: Double = 0.4) = {
    val hsb = toHSB
    val rgb = Colorf.HSBtoRGB(hsb(0), hsb(1), hsb(2) * (1.0 - factor))
    Colorf(rgb(0), rgb(1), rgb(2))
  }
  def opacf(factor: Double): Colorf = Colorf(r, g, b, a * factor)
  def huef(offset: Double): Colorf = {
    val hsb = toHSB
    val rgb = Colorf.HSBtoRGB(hsb(0) + offset, hsb(1), hsb(2))
    Colorf(rgb(0), rgb(1), rgb(2))
  }
  def satf(factor: Double): Colorf = {
    val hsb = toHSB
    val rgb = Colorf.HSBtoRGB(hsb(0), hsb(1) * factor, hsb(2))
    Colorf(rgb(0), rgb(1), rgb(2))
  }
  def toHSB: Array[Float] = Colorf.RGBtoHSB(r, g, b)
  // keep this an option b/c custom color check shouldn't allocate each check
  var converted: AnyRef = _
}
