package memnets.model

import java.util

import scala.beans._
import scala.collection.mutable.ArrayBuffer

/**
 * given such close ties to layer, Plot is in model package not ui
 *
 * should set loc, width, or height until all layers added
 * as such changes are also set on child layers
 *
 * a layer added to multiple plots has undetermined behavior
 * thou Plot.Viz = Skip and setting loc,w,h after should work
 *
 * NOTE: edges use layer.y().loc to know where the
 */
class Plot(val user: Boolean) extends ElementBase {
  private var _range: Option[YRange] = None
  protected val _layers: ArrayBuffer[LayerLike] = ArrayBuffer[LayerLike]()
  @BeanProperty var width: Double = 1000.0
  @BeanProperty var height: Double = 100.0
  @BooleanBeanProperty var useEffect: Boolean = false
  @BooleanBeanProperty var hideGlass: Boolean = false
  @BooleanBeanProperty var showZeroLine: Boolean = true
  @BooleanBeanProperty var showRange: Boolean = true
  @BooleanBeanProperty var autoLayout: Boolean = !user
  @BooleanBeanProperty var stackedBars: Boolean = false
  def add(layers: LayerLike*): Unit = { for (layer <- layers) addHelper(layer) }
  def center(): Unit = {
    autoLayout = false
    loc = Loc().down(height / 2.0)
  }
  def layers: Iterable[LayerLike] = _layers
  def range: YRange = _range.getOrElse(_layers.head.ui.rangeDefault)
  def range_=(value: YRange): Unit = { _range = Option(value) }
  def +=(layer: LayerLike): Unit = { add(layer) }
  def ++=(layers: Iterable[LayerLike]): Unit = { add(layers.toSeq: _*) }
  override def toString: String = s"Plot[layers= ${layers.iterator.map(_.name).mkString(", ")}]"

  protected def addHelper(layer: LayerLike): Unit = {
    _layers += layer
    layer.ui.plot = this
  }

  import collection.JavaConverters._
  def addAll(layers: util.Collection[LayerLike]): Unit = { ++=(layers.asScala) }
  def getLayers: util.Collection[LayerLike] = layers.asJavaCollection
  def getRange: YRange = range
  def setRange(value: YRange): Unit = { range = value }
}

object Plot {
  private[memnets] def apply(layer: LayerLike): Plot = {
    val plot = new Plot(user = false)
    plot.add(layer)
    plot
  }
  def apply(layers: LayerLike*): Plot = {
    apply(layers.toSeq)
  }
  def apply(layers: Seq[LayerLike], showZeroLine: Boolean = false): Plot = {
    val plot = new Plot(user = true)
    plot.add(layers: _*)
    plot.showZeroLine = showZeroLine
    layers.head.system.elements += plot
    plot
  }
}
