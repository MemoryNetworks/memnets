package memnets.ui

import memnets.model._
import memnets.utils.{BeanSupport, DirtySupport}

import scala.beans.BeanProperty
import scala.collection.mutable.ListBuffer

class PhasePlot(
    var scale: Double = 1.0,
    var w: Double = 450,
    var h: Double = 450,
    var zoom: Double = 1.0,
    var showFrame: Boolean = true
)(implicit val mn: DynamicSystem)
    extends ElementBase {

  mn.elements += this
  @BeanProperty var paneStyle: Option[String] = None
  @BeanProperty var autoCamera: AutoCamera = NullAutoCamera
  @BeanProperty var showBorder: Boolean = false
  @BeanProperty var showGlass: Boolean = true
  @BeanProperty var originLight: Boolean = true
  @BeanProperty var sceneRotateY: Double = 0.0
  @BeanProperty var sceneRotateZ: Double = 0.0
  @BeanProperty var tiltDown: Double = 25.0
  var onCreateScene: Scene3D => Unit = sc => {}
  protected[memnets] val _phases = ListBuffer[Phase2D]()
  def phases: Iterable[Phase2D] = _phases
  def width: Double = scale * w
  def height: Double = scale * h
}

object Phase3D {
  def apply(
      x: Yb,
      y: Yb,
      z: Yb,
      window: Int = 360,
      sampling: Int = 1,
      scale: Double = 10.0,
      showAxis: Boolean = true
  )(implicit plot: PhasePlot) = {
    val ph = new Phase3D(x, y, z, plot)
    ph.scale = scale
    ph.window = window
    ph.sampling = sampling
    ph.showAxis = showAxis
    plot.autoCamera = { (te, sc3d) =>
      val scale = ph.scale
      sc3d.moveCamera(-x.act * scale, -y.act * scale, -z.act * scale)
    }
    plot.loc = Loc().right(200)
    ph
  }
}
object Phase {
  def apply(osc: Osc)(implicit plot: PhasePlot): Phase2D = apply(osc.y, osc.x)

  def apply(
      x: Yb,
      y: Yb,
      window: Int = 360,
      sampling: Int = 1,
      scale: Double = 10.0,
      tiltDown: Double = 85.0
  )(implicit plot: PhasePlot) = {

    val ph = new Phase2D(x, y, plot)
    ph.scale = scale
    ph.window = window
    ph.sampling = sampling
    plot.autoCamera = { (te, sc3d) =>
      val scale = ph.scale
      sc3d.moveCamera(-x.act * scale, -y.act * scale)
    }
    plot.tiltDown = tiltDown
    plot.loc = Loc().right(200)
    ph
  }
}
class Phase2D(private var _x: Yb, private var _y: Yb, private val _plot: PhasePlot)
    extends BeanSupport
    with DirtySupport
    with Dsl
    with Logging {

  _plot._phases += this
  @BeanProperty var scale: Double = 10.0
  @BeanProperty var sampling: Int = 2
  @BeanProperty var temporal = false
  @BeanProperty var window = 100
  @BeanProperty var showLabels = true
  @BeanProperty var showAxis = true
  var translateX = 0.0
  def x = _x
  def y = _y
  def plot: PhasePlot = _plot
  def update(osc: Osc): Unit = update(osc.y, osc.x)
  def update(x: Yb, y: Yb): Unit = {
    logger.debug(s"x = ${x.id}, y = ${y.id}")
    require(x != null)
    require(y != null)
    _x = x
    _y = y
    dirty = true
    dirty = false
  }
}
class Phase3D(x: Yb, y: Yb, private var _z: Yb, plot: PhasePlot) extends Phase2D(x, y, plot) {

  def z = _z
  def update(x: Yb, y: Yb, z: Yb): Unit = {
    require(z != null)
    _z = z
    super.update(x, y)
  }
}
