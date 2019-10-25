package memnets.model

object Grid extends Logging {

  /** default is to attempt a square grid based on sqrt(layer.length) */
  def apply(layer: LayerLike): Grid = {
    layer.ui.gridHints
    if (!Math.sqrt(layer.length).isWhole) logger.warn("potential issue for non-integer w,h")
    val rows = Math.floor(Math.sqrt(layer.length)).toInt
    val cols = rows
    val g = new Grid(layer, rows, cols)
    g
  }
}

class Grid(val layer: LayerLike, val rows: Int, val cols: Int) extends ElementBase with GridBase[Yb] {
  name = layer.name
  // layers have loc.y at base (for edges), so need to correct to center of plot
  loc = layer.ui.loc.up(layer.ui.plot.height / 2.0)
  var hints: GridHints = layer.ui.gridHints.getOrElse(GridHints())
  val owner: DynamicSystem = layer.system
  override val length: Int = layer.length
  @inline final def act(te: Tick, r: Int, c: Int): Double = te(layer.id, r * cols + c)
  @inline final def apply(r: Int, c: Int): Yb = layer.y(r * cols + c)
  override def toString(): String = s"Grid[rows= $rows, cols= $cols, lay= ${layer.name}]"
  override def equals(other: Any): Boolean = other match {
    case a: AnyRef => this eq a
    case default   => false
  }
}
