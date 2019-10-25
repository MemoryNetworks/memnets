package memnets.model

object LinkGrid extends Logging {
  type GridKernel = LinkGrid => Unit
  def NullKernel(g: LinkGrid): Unit = {}
  def FullyConnected(g: LinkGrid): Unit = {
    import g._
    // horizontal
    for {
      r <- 0 until rows
      pair <- (0 until cols).sliding(2)
      c = pair(0)
      c2 = pair(1)
    } {
      g.link(r, c, r, c2, 1.0)
      g.link(r, c2, r, c, 1.0)
    }

    // vertical tied
    for {
      c <- 0 until cols
      rowPair <- (0 until rows).sliding(2)
      r = rowPair(0)
      r2 = rowPair(1)
    } {
      g.link(r, c, r2, c, 1.0)
      g.link(r2, c, r, c, 1.0)
    }

    // decay correction factors
    for (c <- 1 until cols - 1) {
      g.link(0, c, 0, c, 1.0) // top row
      g.link(rows - 1, c, rows - 1, c, 1.0) // bottom row
    }
    for (r <- 1 until rows - 1) {
      g.link(r, 0, r, 0, 1.0) // first col
      g.link(r, cols - 1, r, cols - 1, 1.0) // last col
    }
    // correct corners last
    g.link(0, 0, 0, 0, 2.0)
    g.link(0, cols - 1, 0, cols - 1, 2.0)
    g.link(rows - 1, 0, rows - 1, 0, 2.0)
    g.link(rows - 1, cols - 1, rows - 1, cols - 1, 2.0)
  }

  def apply(
      rows: Int,
      cols: Int,
      tau: Double = 30.0,
      decay: Double = -4.0,
      initKernel: GridKernel = LinkGrid.NullKernel
  )(implicit sys: DynamicSystem): Grid = {
    val layer = Layer(n = rows * cols, decay = decay, tau = tau)
    layer.ui.hide()
    val g = new LinkGrid(layer, rows, cols)
    sys.elements += g
    initKernel(g)
    g
  }
}

class LinkGrid(
    override val layer: AbstractLayer,
    override val rows: Int,
    override val cols: Int
) extends Grid(layer, rows, cols) {

  val matrix: SparseLink = layer -%> layer
  def link(r: Int, c: Int, r2: Int, c2: Int, w: Double): Unit = {
    val i = r * cols + c
    val j = r2 * cols + c2
    matrix(j, i) = w
  }
  def pt(r: Int, c: Int): GridPt = {
    new GridPt {
      val index = r * cols + c
      def -->(tgt: GridPt, w: Double): Unit = {
        matrix(tgt.index, this.index) = w
      }
    }
  }
}

trait GridPt {
  def index: Int
  def -->(tgt: GridPt, w: Double): Unit
}
