package memnets.model

trait GridLike[+T] extends IndexedSeq[T] {
  def units: Iterable[(Int, Int, T)] = new IndexedSeq[(Int, Int, T)] {
    def length: Int = rows * cols
    def apply(i: Int): (Int, Int, T) = {
      val r = i / cols
      val c = i % cols
      (r, c, GridLike.this.apply(r, c))
    }
  }
  def upperLeft: T = apply(0, 0)
  def upperRight: T = apply(0, cols - 1)
  def lowerLeft: T = apply(rows - 1, 0)
  def lowerRight: T = apply(rows - 1, cols - 1)
  def corners: Seq[T] = List(upperLeft, upperRight, lowerLeft, lowerRight)
  def rows: Int
  def cols: Int
  def length: Int = rows * cols
  def apply(i: Int): T = apply(i / cols, i % cols)
  def apply(r: Int, c: Int): T
  def subGrid(rDivs: Int, cDivs: Int): GridLike[T] = {
    if (rows <= rDivs && cols <= cDivs)
      this
    else {
      val rDim = if (rDivs < rows) rDivs else rows
      val cDim = if (cDivs < cols) cDivs else cols
      val rStride = Math.max(1.0, (rows - 1).toDouble / (rDim - 1).toDouble)
      val cStride = Math.max(1.0, (cols - 1).toDouble / (cDim - 1).toDouble)
      subGrid(0, 0, rDim, cDim, rStride, cStride, centered = false)
    }
  }
  def subGrid(
      r: Int,
      c: Int,
      rDim: Int,
      cDim: Int,
      rStride: Double = 1.0,
      cStride: Double = 1.0,
      centered: Boolean = true): GridLike[T] = {
    val parent = this
    new GridLike[T] {
      val rOff: Int = if (centered) (-rStride * rDim).toInt / 2 else 0
      val cOff: Int = if (centered) (-cStride * cDim).toInt / 2 else 0
      val rows: Int = rDim
      val cols: Int = cDim
      def apply(i: Int, j: Int): T =
        parent.apply(rOff + r + (i * rStride).asInstanceOf[Int], cOff + c + (j * cStride).asInstanceOf[Int])
    }
  }
}
