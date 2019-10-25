package memnets.fx

import memnets.model._
import scalafx.scene.image.ImageView
import scalafx.scene.shape.Rectangle
import scalafx.scene._

class FrameFX(loc: Loc = Loc(), zoom: Double = 1.0, w: Double = 600, h: Double = 600) extends Logging {
  import scalafx.scene.layout._
  private val ui: JVBox = "frame.fxml".loadFXML
  val fx = new VBox(ui)
  fx.managed = false
  fx.pickOnBounds = true
  val stackPane = new StackPane(ui.findById("stack"))
  val anchorPane = new AnchorPane(ui.findById("main"))
  protected val glass = new Rectangle(ui.findById("glass"))
  val base = new Rectangle(ui.findById("#base"))

  // NOTE : had issue with glass cache not resizing if SPEED set as quality
  glass.cacheHint = CacheHint.Quality
  base.cacheHint = CacheHint.Quality
  glass.cache = true
  base.cache = true

  def hideGlass(): Unit = {
    glass.getStyleClass.removeAll("glass")
    glass.fill = null
    glass.visible = false
  }

  /** must call AFTER hideGlass to work right */
  def showBorder(): Unit = {
    glass.strokeWidth = 2.5
    if (!glass.visible.value) glass.visible = true
  }
  def hideBase(): Unit = base.visibleProperty.setValue(false)

  import scalafx.Includes._
  def root = anchorPane.getChildren.headOption
  def root_=(node: Node): Unit = {
    fx.userData = node.userData
    anchorPane.getChildren.add(node)
    node.managed = true
    for {
      fbs <- node.as[FrameFXStyle]
      style <- fbs.paneStyle
    } anchorPane.style = style

    AnchorPane.clearConstraints(node)
    AnchorPane.setAnchors(node, 0, 0, 0, 0)
    node match {
      case im: ImageView =>
        im.fitWidth <== anchorPane.width
        im.fitHeight <== anchorPane.height
      case default =>
    }
  }
  def initFrame() = {
    // DO NOT CALL fx.layout()
    fx.scaleX = zoom
    fx.scaleY = zoom
    val off = glass.strokeWidth.value
    fx.relocate(
      loc.x - fx.scaleX.value * (off + anchorPane.prefWidth.value / 2.0),
      loc.y - fx.scaleY.value * (off + anchorPane.prefHeight.value / 2.0))
  }
  def resetFrame(): Unit = {
    anchorPane.prefWidthProperty.setValue(w)
    glass.widthProperty.setValue(w)
    base.widthProperty.setValue(w)

    anchorPane.prefHeightProperty.setValue(h)
    glass.heightProperty.setValue(h)
  }
  resetFrame()
}
