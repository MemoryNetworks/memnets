package memnets.fx

import memnets.fx.TimeChartFX.S
import memnets.model._
import memnets.ui._
import scalafx.scene.chart.LineChart
import scalafx.scene.layout.StackPane
import scalafx.scene.paint._

class ChartSkinFX extends SkinFX with Logging {
  name = "Chart"
  backImageOn = false
  backColor = Colorf.WHITE
  topPaneOn = false
  customTimeChart = true
  class TimeChartPFX(
      val pane: StackPane,
      override val timeChart: TimeChart,
      override val chartFX: javafx.scene.chart.LineChart[Number, Number],
      override val yColorMap: YColorMap[Color]
  ) extends TimeChartFX(timeChart, chartFX, yColorMap) {
    override protected def addSeries(y: Trackable, series: S): Unit = {
      super.addSeries(y, series)
      val pos = chartSeriesData.indexOf(series)
      changeColor(pos, yColorMap.trackColor(y).toRGBA)
    }
    private def changeColor(position: Int, color: String): Unit = {
      runLaterP {
        val lineSym = chartFX.lookup(".default-color" + position + ".chart-line-symbol")
        val legendSym = chartFX.lookup(".default-color" + position + ".chart-legend-item-symbol")
        if (lineSym != null) lineSym.setStyle("-fx-background-color: " + color + ", white;")
        if (legendSym != null) legendSym.setStyle(s"-fx-background-color: $color, $color;")
      }
    }
    override def init(): Unit = {
      super.init()
      pane.prefWidth = Display.width
      pane.prefHeight = Display.height
      pane.relocate(0.0, 0.0)
      pane.layout()
    }
    override def node = pane
    override def initChart(): Unit = {
      chartFX.setLegendVisible(true)
    }
    override def initYAxis(): Unit = {
      yAxis.setAnimated(false)
      yAxis.setUpperBound(1.04)
      yAxis.setLowerBound(-0.04)
      yAxis.setAutoRanging(true)
    }
    override def updateYAxis(min: Double, max: Double): Unit = {}
  }
  override def create(elem: Element): Option[TickableFX] = elem match {
    case tc: TimeChart =>
      val ui: JStackPane = "chart.fxml".loadFXML
      val fx = new StackPane(ui)
      val lineChart = new LineChart[Number, Number](ui.findById("chart"))
      new TimeChartPFX(fx, tc, lineChart, yColorMap)
    case default =>
      None
  }
  override def createY(y: Y) = None
}
