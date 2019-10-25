package memnets.model

import java.util.Optional

trait UIable[+T] {
  def ui: T
  def getUI: T = ui
}
trait TrackableUI {
  def range: YRange
  def scale: Option[Float]
}
trait ElementUI extends Locatable {
  def viz: Viz
  def viz_=(v: Viz): Unit
  def hide(): Unit = { viz = Viz.Hide }
  def skip(): Unit = { viz = Viz.Skip }
  def show(): Unit = { viz = Viz.Default }
  def isShown: Boolean = viz > Viz.Hide
  def isHidden: Boolean = viz == Viz.Hide
  def isSkipped: Boolean = viz <= Viz.Skip
  // Java
  def getViz: Viz = viz
  def setViz(v: Viz): Unit = viz = v
}
trait YbUI extends ElementUI with TrackableUI {
  def scale: Option[Float]
}
trait YUI extends YbUI with UIBase { self: Config =>
  def owner: Y
  def color: Option[Colorf] = owner.get[Colorf](Config.COLF)
  def color_=(c: Colorf): Unit = { owner.update(Config.COLF, c) }
  def range: YRange = rangeDefault
}

trait UIBase { self: Config =>
  import Config._
  def minZero: Boolean =
    normalized || activation == Activation.Relu || activation == Activation.Sigmoid || hasFlag(MIN_ZERO) || hasFlag(
      RELU_MAX)
  def minZero_=(b: Boolean): Unit = setFlag(MIN_ZERO, b)
  def normalized: Boolean =
    hasFlag(NORMALIZED) || activation == Activation.Sigmoid || relumax.map(_ ~ 1.0f).getOrElseP(false)
  def normalized_=(b: Boolean): Unit = { setFlag(NORMALIZED, b) }
  def scale: Option[Float] = {
    val sc = get[Float](SCALE)
    if (sc.isDefined)
      sc
    else if (normalized)
      ONE
    else
      None
  }
  def scale_=(v: Double): Unit = { update(SCALE, v) }
  def showText: Boolean = hasFlag(SHOW_TEXT)
  def showText_=(show: Boolean): Unit = { setFlag(SHOW_TEXT, show) }
  def rangeDefault: YRange = {
    if (normalized)
      NormalizedRange
    else {
      val sc = get[Float](Config.SCALE)
      YRange(
        min = if (minZero) 0.0 else sc.map(-1.0f * _).getOrElseP(-YRange.scaleF).toDouble,
        max = relumax.getOrElse(sc.getOrElseP(YRange.scaleF)).toDouble
      )
    }
  }
  // Java
  def isMinZero: Boolean = minZero
  def setMinZero(b: Boolean): Unit = minZero = b
  def isNormalized: Boolean = normalized
  def setNormalized(b: Boolean): Unit = normalized = b
  def getScale: Option[Float] = scale
  def setScale(s: Double): Unit = scale = s
  def isShowText: Boolean = showText
  def setShowText(b: Boolean): Unit = showText = b
}

trait LayerLikeUI extends ElementUI {
  def format: IndexFormat
  def format_=(f: IndexFormat): Unit
  def gradient: Option[GradientHints]
  def gradient_=(hints: GradientHints): Unit

  /** if layer rendered as grid, optional hints */
  def gridHints: Option[GridHints]
  def gridHints_=(value: GridHints): Unit
  def numericalType: VariableType
  def numericalType_=(hint: VariableType): Unit
  def owner: LayerLike
  def plot: Plot
  def plot_=(plot: Plot): Unit

  /** NOTE: this can potentially be expensive, so cache locally */
  def rangeDefault: YRange
  def showText: Boolean
  def spacing(w: Double): Double = {
    if (numericalType == VariableType.Continuous)
      w / (owner.length - 1).toDouble
    else
      w / owner.length.toDouble
  }
  // Java
  import memnets.utils.JavaUtils._
  def getHints: Optional[GradientHints] = gradient
  def setHint(value: GradientHints): Unit = { gradient = value }
  def getImageHints: Option[GridHints] = gridHints
  def setImageHints(value: GridHints): Unit = { gridHints = value }
  def getFormat = format
  def setFormat(value: IndexFormat): Unit = { format = value }
  def getNumericalType: VariableType = numericalType
  def setNumericalType(value: VariableType): Unit = { numericalType = value }
  def getRangeDefault: YRange = rangeDefault
  def getPlot = plot
  def setPlot(value: Plot): Unit = { plot = value }
  def getOwner = owner
}
trait IndexFormat {
  def format(index: Int, act: Double): String
}
trait LayerUI extends LayerLikeUI with UIBase { self: Config =>
  override def owner: AbstractLayer
  def getX(i: Int): Double
}
