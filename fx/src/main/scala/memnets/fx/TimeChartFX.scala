package memnets.fx

import java.lang

import javafx.util.StringConverter
import memnets.model._
import memnets.ui._
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color

import scala.collection.mutable.ArrayBuffer

object TimeChartFX extends Logging {
  type Z = javafx.scene.chart.XYChart.Data[Number, Number]
  type S = javafx.scene.chart.XYChart.Series[Number, Number]
  val ZEROD = lang.Double.valueOf(0.0)
  final class XConverter(sampling: Int) extends StringConverter[Number] {
    var t = 0
    def fromString(n: String): Number = Integer.valueOf(n)
    @inline def toString(n: Number): java.lang.String = {
      val x = sampling * n.intValue + t
      val xInt = x / 60
      if (xInt < 0) "" else xInt + "s"
    }
  }
  final class YConverter extends StringConverter[Number] {
    val df = memnets.utils.OneDigit
    val ZERO = "0.0"
    def fromString(n: String): Number = java.lang.Float.valueOf(n)
    @inline def toString(n: Number): java.lang.String = {
      val y = n.floatValue
      if (y >= 0.1f) df.format(y)
      else if (y <= -0.1f) df.format(y)
      else ZERO
    }
  }
}

/**
 * NOTE: had a very hard time with dynamic charts performance.
 * using trick of changing Y values only and faking labels for static.
 * this breaks if fastforward past chart sampling rate
 * for default value sampling = 4, can FF up to 4x
 */
class TimeChartFX(
    val timeChart: TimeChart,
    val chartFX: javafx.scene.chart.LineChart[Number, Number],
    val yColorMap: YColorMap[Color]
) extends TickableFX
    with Logging {
  import TimeChartFX._
  import scalafx.Includes._
  require(yColorMap != null) // blow up on ctor

  val fx = chartFX
  def node: Option[Node] = None // don't all to ElemGroup

  chartFX.setFocusTraversable(false)
  chartFX.setAnimated(false) // very important
  chartFX.setCreateSymbols(false)
  val maxSamples = 120 // should be div by 60
  val sampling = timeChart.sampling
  val chartSeriesData = new ObservableBuffer[S]()
  chartFX.setData(chartSeriesData)
  val xAxis = chartFX.getXAxis.asInstanceOf[javafx.scene.chart.NumberAxis]
  val yAxis = chartFX.getYAxis.asInstanceOf[javafx.scene.chart.NumberAxis]

  val xConverter = new XConverter(sampling)
  val yConverter = new YConverter
  xAxis.setAnimated(false)
  xAxis.setAutoRanging(false)
  xAxis.setUpperBound(0)
  xAxis.setLowerBound(-maxSamples)
  xAxis.setTickUnit(maxSamples / 4)
  xAxis.setMinorTickCount(1)

  yAxis.setAnimated(false)

  def initChart(): Unit = {
    chartFX.setLegendVisible(false)
  }
  def initYAxis(): Unit = {
    yAxis.setAutoRanging(false)
    yAxis.setUpperBound(1.0 + padY)
    yAxis.setLowerBound(0.0 - padY)
    yAxis.tickUnitProperty <== (yAxis.upperBoundProperty - yAxis.lowerBoundProperty) / 4.0
  }
  def updateYAxis(min: Double, max: Double): Unit = {
    if (!timeChart.dynamicRange.value) {
      if (max > yAxis.getUpperBound)
        yAxis.setUpperBound(max)
      if (min < yAxis.getLowerBound)
        yAxis.setLowerBound(min)
    } else {
      yAxis.setUpperBound(max + padY)
      yAxis.setLowerBound(min - padY)
    }
  }
  yAxis.setTickLabelFormatter(yConverter)

  type P = java.util.LinkedList[lang.Double]
  val y2Series = ArrayBuffer[(Trackable, S, P)]()
  override def init(): Unit = {
    super.init()
    initChart()
    initYAxis()
    for (n <- timeChart.tracked)
      track(n)
  }
  override def reset(): Unit = {
    timeChart.reset()
    for ((y, series, list) <- y2Series) {
      list.clear()
      val data = series.getData
      for (i <- 0 until maxSamples) {
        data.get(i).setYValue(ZEROD)
        list.add(ZEROD)
      }
    }
    yAxis.setUpperBound(1.0 + padY)
    yAxis.setLowerBound(0.0 - padY)
    for (y <- y2Series.map(_._1)) updateBounds(y)
    refreshXAxis(0)
  }
  @inline final def hasSpike: Boolean = timeChart.tracked.exists(_.isSpiking)
  final def tick(te: Tick): Unit = {
    val t = te.t
    // tick.0 can come from a dirty refresh, but don't want bad samples
    if (chartFX.isVisible && t > 0 && (t % sampling == 0 || hasSpike)) {
      var min = Double.MaxValue
      var max = Double.MinValue
      val len = y2Series.length
      var i = 0
      while (i < len) {
        val (y, series, list) = y2Series(i)
        val act = y.act
        val act2 = if (Math.abs(act) < 0.01) 0.0 else act
        list.remove(0)
        list.add(lang.Double.valueOf(act2))

        var j = maxSamples - 1
        val data = series.getData
        val iter = list.iterator()
        while (iter.hasNext) {
          val pt = iter.next()
          data.get(j).setYValue(pt)
          val d = pt.doubleValue
          if (d > max) max = d
          if (d < min) min = d
          j -= 1
        }
        i += 1
      }
      updateYAxis(min, max)
      if (t % 60 == 0)
        refreshXAxis(t)
    }
  }
  private def refreshXAxis(t: Int): Unit = {
    xConverter.t = t
    xAxis.setTickLabelFormatter(null)
    xAxis.setTickLabelFormatter(xConverter)
  }
  protected def track(y: Trackable): Unit = {
    if (!y2Series.exists(_._1 == y)) {
      val series = new S()
      series.setName(y.toString)
      val list = new P()
      for (i <- 0 until maxSamples) {
        series.getData.add(new Z(-i, ZEROD))
        list.add(ZEROD)
      }
      val tuple = (y, series, list)
      y2Series += tuple
      // must do b4 add series...
      series.nodeProperty ==> { n =>
        if (n != null) n.setStyle(nStyle(y))
      }
      addSeries(y, series)
      updateBounds(y)
    }
  }
  protected def addSeries(y: Trackable, series: S): Unit = { chartSeriesData += series }
  def updateBounds(y: Trackable): Unit = {
    val range = y.ui.range
    if (range.max > yAxis.getUpperBound) {
      logger.trace(s"upper bound: $y= ${range.max}")
      yAxis.setUpperBound(range.max + padY)
    }
    if (range.min < yAxis.getLowerBound) {
      logger.trace(s"lower bound: $y= ${range.min}")
      yAxis.setLowerBound(range.min - padY)
    }
  }
//  val padY = 0.04
  val padY = 0.00
  override def destroy(): Unit = {
    reset
    subSelected.cancel()
    chartSeriesData.clear
    y2Series.clear
  }
  def nStyle(n: Trackable) = {
    val col = yColorMap.trackColor(n)
    s"-fx-stroke: ${col.toRGB}; -fx-stroke-width: ${if (timeChart.selected.value == null) "2.5"
    else if (n == timeChart.selected.value) "3.5"
    else "1.0"};"
  }
  val subSelected = timeChart.selected ==> { sel =>
    for ((y, s, list) <- y2Series)
      s.getNode.setStyle(nStyle(y))
  }
}
