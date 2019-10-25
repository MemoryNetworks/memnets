package memnets.fx

import javafx.embed.swing.SwingFXUtils
import memnets.awt.ImageAwt
import memnets.model._
import memnets.ui.ColorMap
import scalafx.geometry.Point2D
import scalafx.scene.CacheHint
import scalafx.scene.image._
import scalafx.scene.layout.StackPane
import scalafx.scene.shape.Rectangle

class ImageFX(val grid: GridData, colMap: ColorMap[java.awt.Color]) extends FullSceneFX with Logging {
  val imageAwt = new ImageAwt(grid, colMap)
  val ui: JStackPane = "image.fxml".loadFXML
  val fx = new StackPane(ui) {
    managed = true
    pickOnBounds = true
  }
  val plot = new ImageView(fx.children.get(0).asInstanceOf[JImageView])
  val glass = new Rectangle(fx.children.get(1).asInstanceOf[JRectangle])
  val base = new Rectangle(fx.children.get(2).asInstanceOf[JRectangle])
  val image = new WritableImage(width = grid.w, height = grid.h)
  var viz = Viz.Default

  glass.mouseTransparent = true
  plot.image = image
  glass.width <== plot.fitWidth
  glass.height <== plot.fitHeight
  base.width <== plot.fitWidth

  def node = fx
  override def mouseNode = plot
  override def destroy(): Unit = { imageAwt.destroy() }
  override def init(): Unit = {
    super.init()
    plot.opacity = grid.hints.opacity
    if (!grid.hints.showGlass) hideGlass
    if (!grid.hints.showBorder) hideBorder
    if (!grid.hints.reflection) fx.effect = null

    val w = grid.hints.width
    val h = grid.hints.height
    plot.fitWidth = w
    plot.fitHeight = h
    fx.scaleX.value = grid.hints.scaleX
    fx.scaleY.value = grid.hints.scaleY
    fx.relocate(grid.ui.loc.x - w / 2.0, grid.ui.loc.y - h / 2.0)

    // NOTE : had issue with glass cache not resizing if SPEED set as quality
    glass.cacheHint = CacheHint.Quality
    base.cacheHint = CacheHint.Quality
    glass.cache = true
    base.cache = true
  }
  def hideGlass: Unit = {
    glass.getStyleClass.removeAll("glass")
    glass.fill = null
  }
  def showBorder: Unit = {
    glass.strokeWidth = 4.0
    if (!glass.visible.value) glass.visible = true
  }
  def hideBorder: Unit = {
    glass.strokeWidth = 0.0
  }
  override def reset(): Unit = {
    super.reset()
    imageAwt.reset()
  }
  def tick(te: Tick): Unit = {
    if (grid.preRender(te)) {
      imageAwt.tick(te)
      SwingFXUtils.toFXImage(imageAwt.bufferedImage, image)
    }
  }
  def fullWidth = plot.fitWidth
  def fullHeight = plot.fitHeight
  def relocate(x: scala.Double, y: scala.Double) = fx.relocate(x, y)
  override def findTarget(x: Double, y: Double): Option[UserSource] = {
    val bnds = plot.getBoundsInLocal
    val wD = bnds.getWidth
    val hD = bnds.getHeight
    val pt0 = plot.localToParent(new Point2D(x, y))
    val pt = fx.localToParent(pt0)
    logger.debug(s"w = $wD, h = $hD, x = $x, y = $y, ptX = ${pt.getX}, ptY = ${pt.getY}, me = ${x}")
    val loc = Loc(pt.getX, pt.getY)
    grid match {
      case g: GridBase[Yb] =>
        logger.debug("grid")
        var r = (y / hD * g.rows).toInt
        r = if (r < 0) 0 else if (r > g.rows - 1) g.rows - 1 else r
        var c = (x / wD * g.cols).toInt
        c = if (c < 0) 0 else if (c > g.cols - 1) g.cols - 1 else c
        val tgt = g(r, c)
        Some(UserSource(tgt, loc = loc, h = loc.y - (pt.getY - pt0.getY)))
      case default =>
        None
    }
  }
  fx.onContextMenuRequested = me => {
    if (grid.hints.fixedDimension)
      toggleFullScene()
    me.consume()
  }
}
