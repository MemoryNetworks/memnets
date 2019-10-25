package memnets.fx

import javafx.scene.shape.StrokeLineCap
import memnets.model.{DEFAULT_PRECISON => _, _}
import memnets.utils.{FastFormatter, OneDigit, _}
import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Arc
import scalafx.scene.text.Text

trait GaugeBase {
  def reset: Unit
  def max: Double
  @inline final def arcConvert(v: Double): Double = {
    if (v > 0.0)
      Math.max(GaugeFX.MAX_ANGLE, (v / max) * GaugeFX.MAX_ANGLE)
    else
      0.0
  }
}
object GaugeFX {
  val arcPrefSize = 132.0
  val arcRadius = 56.0
  val arcStroke = 10.0
  val MAX_ANGLE = -270.0
  val precOne = Precision(0.1)
  val precTwo = Precision(0.2)
  val precRound = Precision(0.9)
  val threshColor = Color.White.opacity(0.7)
  val REDZONE_COLOR = Color.Red.opacity(0.5)
  @inline final def changed(prior: Double, v: Double, format: FastFormatter): Boolean = {
    val prec =
      if (format == OneDigit)
        precOne
      else if (format == ZeroDigits)
        precRound
      else if (format == TwoDigits)
        precTwo
      else
        precTwo
    prior.!~(v)(prec) || (Math.abs(v) < 0.01 && Math.abs(prior) > 0.00)
  }
  def apply(
      title: String,
      units: String = "",
      max: Double = 100.0,
      format: FastFormatter = OneDigit,
      color: Color = Color.LightGray): GaugeFX = {
    val gauge = new GaugeFX()
    gauge.title.value = title
    gauge.units.value = units
    gauge.max = max
    gauge.format = format
    gauge.setColor(color)
    gauge
  }
}

class GaugeFX extends StackPane("gauge.fxml".loadFXML) with GaugeBase {
  import GaugeFX._
  protected val gaugeText = new Text(this.findTextById("gaugeText"))
  protected val titleText = new Text(this.findTextById("titleText"))
  protected val unitsText = new Text(this.findTextById("unitsText"))
  protected val arc = new Arc(this.findById("gaugeArc"))
  arc.opacity = 0.5
  protected val arcPane = new AnchorPane(this.findById("arcPane"))
  protected val glass = new Arc(this.findById("arcGlass"))
  protected val thresh = new Arc(this.findById("arcThresh"))
  var max: Double = 100.0
  var format: FastFormatter = OneDigit
  def units = unitsText.text
  def title = titleText.text
  def color = arc.stroke

  def reset: Unit = {
    setValue(0.0)
  }
  def setColor(c: Color): Unit = { arc.stroke.value = c }
  def setThreshold(
      thr: Double,
      redzone: Boolean = false,
      nonredcolor: Color = threshColor,
      redzoneColor: Option[Color] = None): Unit = {
    thresh.visible = true
    if (redzone) {
      thresh.delegate.setStartAngle(-90.0 + arcConvert(thr))
      thresh.delegate.setStroke(redzoneColor.getOrElseP(REDZONE_COLOR))
      thresh.delegate.setLength(arcConvert(max) - arcConvert(thr))
      thresh.delegate.setStrokeLineCap(StrokeLineCap.SQUARE)
    } else {
      thresh.delegate.setStartAngle(-90.0 + arcConvert(thr + 3.0)) // for some reason, need to account for width...
      thresh.delegate.setStroke(nonredcolor)
      thresh.delegate.setLength(-3.0)
      thresh.delegate.setStrokeLineCap(StrokeLineCap.BUTT)
    }
  }
  // corrections
  glass.length = MAX_ANGLE
  private var realVal = 1.0
  val value = DoubleProperty(1.0) // set value to zero below, so want diff...
  value.onChange { (_, _, vN) =>
    val v = vN.doubleValue
    if (changed(realVal, v, format)) {
      realVal = v
      gaugeText.delegate.setText(format.format(v))
      arc.delegate.setLength(arcConvert(v))
    }
  }
  def setValue(v: Double): Unit = {
    if (changed(realVal, v, format)) value.value = v
  }
  setValue(0.0)
  def setGaugeSize(size: Double): Unit = {
    val scale = size / arcPrefSize
    arcPane.setPrefSize(scale * arcPrefSize, scale * arcPrefSize)

    val center = scale * arcPrefSize / 2.0
    glass.centerX = center
    glass.centerY = center
    arc.centerX = center
    arc.centerY = center
    val stroke = scale * arcStroke
    glass.strokeWidth = stroke
    arc.strokeWidth = stroke
    val radius = scale * arcRadius
    glass.radiusX = radius
    glass.radiusY = radius
    arc.radiusX = radius
    arc.radiusY = radius
  }
}

object DualGaugeFX {
  def apply(
      title: String,
      units: String = "",
      max: Double = 100.0,
      format: FastFormatter = OneDigit,
      color: Color = Color.LightGray,
      color2: Color = Color.DarkGray): DualGaugeFX = {
    val gauge = new DualGaugeFX()
    gauge.title.value = title
    gauge.units.value = units
    gauge.max = max
    gauge.format = format
    gauge.setColors(color, color2)
    gauge
  }
}

class DualGaugeFX extends StackPane("dualgauge.fxml".loadFXML) with GaugeBase {
  import GaugeFX._
  protected val text = new Text(this.findTextById("gaugeText"))
  protected val titleText = new Text(this.findTextById("titleText"))
  protected val unitsText = new Text(this.findTextById("unitsText"))
  protected val arc = new Arc(this.findById("arc"))
  protected val arc2 = new Arc(this.findById("arc2"))
  protected val arcPane = new AnchorPane(this.findById("arcPane"))
  protected val glass = new Arc(this.findById("arcGlass"))
  var max: Double = 100.0
  var format: FastFormatter = OneDigit
  def title = titleText.text
  def units = unitsText.text
  def color = arc.stroke
  def reset: Unit = { setValues(0.0, 0.0) }
  def setColors(c: Color, c2: Color): Unit = {
    arc.stroke.value = c
    arc2.stroke.value = c2
  }
  // corrections
  glass.length = MAX_ANGLE
  private var realVal, realVal2 = 1.0
  def setValues(v: Double, v2: Double): Unit = {
    if (changed(realVal, v, format) || changed(realVal2, v2, format)) {
      realVal = v
      realVal2 = v2
      text.delegate.setText(format.format(v) + "/" + format.format(v2))
      arc.delegate.setLength(arcConvert(v))
      arc2.delegate.setLength(arcConvert(v2))
    }
  }
  setValues(0.0, 0.0)
  def setGaugeSize(size: Double): Unit = {
    val scale = size / arcPrefSize
    arcPane.setPrefSize(scale * arcPrefSize, scale * arcPrefSize)

    val center = scale * arcPrefSize / 2.0
    glass.centerX = center
    glass.centerY = center
    arc.centerX = center
    arc.centerY = center
    val stroke = scale * arcStroke
    glass.strokeWidth = stroke
    arc.strokeWidth = stroke
    val radius = scale * arcRadius
    glass.radiusX = radius
    glass.radiusY = radius
    arc.radiusX = radius
    arc.radiusY = radius
  }
}
object DialGaugeFX {
  def apply(
      title: String,
      units: String = "",
      max: Double = 100.0,
      format: FastFormatter = ZeroDigits,
      color: Color = Color.White.opacity(0.9),
      showText: Boolean = true): DialGaugeFX = {
    val gauge = new DialGaugeFX()
    gauge.title.value = title
    gauge.units = units
    gauge.max = max
    gauge.format = format
    gauge.text.visible = showText
    gauge.setColor(color)
    gauge
  }
}
class DialGaugeFX extends StackPane("dialgauge.fxml".loadFXML) with GaugeBase {
  import GaugeFX._
  protected val text = new Text(this.findTextById("gaugeText"))
  protected val angle = new Text(this.findTextById("angleText"))
  val label = new Text(this.findTextById("labelText"))
  protected val arc = new Arc(this.findById("arc"))
  protected val arcPane = new AnchorPane(this.findById("arcPane"))
  protected val glass = new Arc(this.findById("arcGlass"))
  protected val thresh = new Arc(this.findById("arcThresh"))
  var max: Double = 45.0
  var format: FastFormatter = OneDigit
  var units: String = ""
  def title = label.text
  def color = arc.stroke

  def reset: Unit = {
    setAngle(0.0)
    setValue(0.0)
  }
  def setColor(c: Color): Unit = { arc.stroke.value = c }

  private var realVal = 1.0
  private var realAngle = 1.0
  def setValue(v: Double): Unit = {
    if (changed(realVal, v, format)) {
      realVal = v
      text.delegate.setText(format.format(v) + units)
    }
  }
  def setAngle(v: Double): Unit = {
    if (changed(realAngle, v, OneDigit)) {
      realAngle = v
      angle.delegate.setText(OneDigit.format(v) + "\u00b0")
      var ang = 88.0 + -v / max * 140.0
      if (ang > 230.0)
        ang = 230.0
      else if (ang < -50.0)
        ang = -50.0

      arc.delegate.setStartAngle(ang)
    }
  }
  setValue(0.0)
  def setGaugeSize(size: Double): Unit = {
    val scale = size / arcPrefSize
    arcPane.setPrefSize(scale * arcPrefSize, scale * arcPrefSize)

    val center = scale * arcPrefSize / 2.0
    glass.centerX = center
    glass.centerY = center
    arc.centerX = center
    arc.centerY = center
    val stroke = scale * arcStroke
    glass.strokeWidth = stroke
    arc.strokeWidth = stroke
    val radius = scale * arcRadius
    glass.radiusX = radius
    glass.radiusY = radius
    arc.radiusX = radius
    arc.radiusY = radius
  }
}
