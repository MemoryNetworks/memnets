package memnets.model

trait GridBase[+T <: Yb] extends GridLike[T] with GridData {
  def owner: DynamicSystem
  def act(te: Tick, r: Int, c: Int): Double
  def yScale: Float = apply(0).ui.scale.getOrElse(YRange.scaleF)

  override def subGrid(rDivs: Int, cDivs: Int): GridBase[T] = {
    val rDim = if (rDivs < rows) rDivs else rows
    val cDim = if (cDivs < cols) cDivs else cols
    val rStride = Math.max(1.0, (rows - 1).toDouble / (rDim - 1).toDouble)
    val cStride = Math.max(1.0, (cols - 1).toDouble / (cDim - 1).toDouble)
    subGrid(0, 0, rDim, cDim, rStride, cStride, centered = false)
  }
  override def subGrid(
      r: Int,
      c: Int,
      rDim: Int,
      cDim: Int,
      rStride: Double = 1.0,
      cStride: Double = 1.0,
      centered: Boolean = true): GridBase[T] = {
    val parent = this
    object sub extends ElementBase with GridBase[T] {
      name = "sub" + parent.name
      loc = parent.ui.loc
      val rOff: Int = if (centered) (-rStride * rDim).toInt / 2 else 0
      val cOff: Int = if (centered) (-cStride * cDim).toInt / 2 else 0
      val rows: Int = rDim
      val cols: Int = cDim
      def apply(i: Int, j: Int): T = {
        val r2 = rOff + r + (i * rStride).asInstanceOf[Int]
        val c2 = cOff + c + (j * cStride).asInstanceOf[Int]
        parent.apply(r2, c2)
      }
      override def preRender(te: Tick): Boolean = parent.preRender(te)
      def act(te: Tick, i: Int, j: Int): Double =
        parent.act(te, rOff + r + (i * rStride).asInstanceOf[Int], cOff + c + (j * cStride).asInstanceOf[Int])
      def owner: DynamicSystem = parent.owner
      def hints: GridHints = parent.hints
      def hints_=(hints: GridHints): Unit = parent.hints = hints
    }
    sub
  }
}
