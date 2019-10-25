package memnets.utils

trait FastFormatter {
  def format(d: Double): String
}
object OneDigit extends FastFormatter {
  @inline def format(d: Double): String = {
    val s = String.valueOf(d)
    val i = s.indexOf('.') + 2
    s.substring(0, if (i < s.length) i else s.length)
  }
}
object TwoDigits extends FastFormatter {
  @inline def format(d: Double): String = {
    val s = String.valueOf(d)
    val i = s.indexOf('.') + 3
    s.substring(0, if (i < s.length) i else s.length)
  }
}
object ZeroDigits extends FastFormatter {
  @inline def format(d: Double) = d.asInstanceOf[Int].toString
}
