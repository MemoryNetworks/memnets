package memnets.fx

import memnets.model._
import scalafx.scene._
import scalafx.scene.control._

import scala.collection.mutable.ListBuffer

class ContainerFX(
    val tfx: TickableFX,
    override var loc: Loc,
    zoom: Double = 1.0,
    override val minusHeader: Boolean = true
) extends ElementBase
    with FullSceneFX
    with Logging {

  def this(tfx: TickableFX with Locatable) {
    this(tfx, tfx.loc)
  }
  protected var _headerOpacity: Option[Double] = None

  val fx = tfx.node.get
  tfx.init() // for bounds b4 FrameFX ctor
  val bnds = fx.getBoundsInParent
  logger.debug("bnds: w= " + bnds.getWidth + ", h= " + bnds.getHeight)
  var fw = fx.getBoundsInLocal.getWidth
  if (fw == 0) fw = fx.prefWidth(600.0)
  var fh = fx.getBoundsInLocal.getHeight
  if (fh == 0) fh = fx.prefHeight(400.0)
  val frame = new FrameFX(loc = loc, zoom = zoom, w = fw, h = fh)
  //  val frame = FrameFX(loc = loc, zoom = zoom, w = fx.prefWidth(600.0), h = fx.prefHeight(400))
  logger.debug(s"frame = $frame")
  frame.root = fx

  def node = frame.fx
  def tick(te: Tick) = tfx.tick(te)
  // will reset any fx.relocate in tickable.init
  override def init(): Unit = { frame.initFrame }
  override def reset(): Unit = { tfx.reset() }
  def fullWidth = frame.anchorPane.prefWidth
  def fullHeight = frame.anchorPane.prefHeight
  def relocate(x: scala.Double, y: scala.Double) = frame.anchorPane.relocate(x, y)

  // FullScene
  def headerOpacity_=(op: Double) = _headerOpacity = Some(op)
  override def headerOpacity: Option[Double] = _headerOpacity
  override def fullSceneNode: Option[Node] = {
    val n = frame.anchorPane
    if (frame.stackPane.children.contains(n))
      frame.stackPane.children.remove(n)
    n
  }
  override def unbind(): Unit = {
    super.unbind()
    val n = frame.anchorPane
    if (!frame.stackPane.children.contains(n)) {
      frame.stackPane.children.add(n)
      n.toBack()
    }
    frame.resetFrame
  }

  if (!fx.isInstanceOf[SubScene]) {
    fx.onContextMenuRequested = me => {
      val items = ListBuffer[MenuItem]()
      items += new MenuItem("Toggle FullScreen") {
        onAction = e => { toggleFullScene() }
      }
      val menu = new ContextMenu(items.toSeq: _*) {
        autoHide = true
        show(frame.anchorPane, me.getScreenX, me.getScreenY)
      }
      me.consume()
    }
  }
}
