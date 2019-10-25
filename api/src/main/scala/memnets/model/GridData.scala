package memnets.model

trait GridData extends Element {
  def rows: Int
  def cols: Int
  def preRender(te: Tick) = true
  def act(te: Tick, r: Int, c: Int): Double
  def reset(): Unit = {}
  def yScale: Float
  def hints: GridHints
  def hints_=(hints: GridHints): Unit
  def w: Int = cols
  def h: Int = rows

  def subGridData(rDivs: Int, cDivs: Int): GridData = {
    if (rows <= rDivs && cols <= cDivs)
      this
    else {
      val rDim = if (rDivs < rows) rDivs else rows
      val cDim = if (cDivs < cols) cDivs else cols
      val rStride = Math.max(1.0, (rows - 1).toDouble / (rDim - 1).toDouble)
      val cStride = Math.max(1.0, (cols - 1).toDouble / (cDim - 1).toDouble)
      val parent = this
      object sub extends ElementBase with GridData {
        name = "sub" + parent.name
        loc = parent.ui.loc
        val rows: Int = rDim
        val cols: Int = cDim
        override def preRender(te: Tick): Boolean = parent.preRender(te)
        def act(te: Tick, r: Int, c: Int): Double = {
          parent.act(te, (r * rStride).asInstanceOf[Int], (c * cStride).asInstanceOf[Int])
        }
        override def reset(): Unit = parent.reset()
        def yScale: Float = parent.yScale
        def hints: GridHints = parent.hints
        def hints_=(hints: GridHints): Unit = { parent.hints = hints }
      }
      sub
    }
  }
}
