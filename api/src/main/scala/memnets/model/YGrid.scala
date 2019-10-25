package memnets.model

object YGrid {
  def NullKernel(g: YGrid): Unit = {}
  def FullyConnected(g: YGrid): Unit = {
    import g._
    implicit val dsys = owner
    // horizontal
    for {
      row <- rowData
      pair <- row.sliding(2)
      y1 = pair(0)
      y2 = pair(1)
    } {
      y1 --> y2
      y2 --> y1
    }
    // vertical tied
    for {
      rowPair <- rowData.sliding(2)
      row = rowPair.head
      row2 = rowPair.last
      (x, x2) <- row.zip(row2)
    } {
      x --> x2
      x2 --> x
    }
    // correction factors
    for (y <- rowData.head)
      y.decay.get.w = -3.0 // top row
    for (y <- rowData.last)
      y.decay.get.w = -3.0 // bottom row
    for (row <- rowData) {
      row.head.decay.get.w = -3.0 // first col
      row.last.decay.get.w = -3.0 // last col
    }
    // correct corners last
    for (corner <- corners) corner.decay.get.w = -2.0
  }
}

case class YGrid(
    rows: Int,
    cols: Int,
    tau: Double = 30.0,
    decay: Double = -4.0,
    private val initKernel: YGrid => Unit = YGrid.NullKernel)(implicit sys: DynamicSystem)
    extends ElementBase
    with GridBase[Y] {

  sys.elements += this
  var hints = GridHints()
  def scale: Float = upperLeft.ui.scale.getOrElseP(YRange.scaleF)
  def create(r: Int, c: Int): Y = {
    val y = Y(decay = decay, tau = tau)
    y.ui.hide()
    y
  }
  def owner: DynamicSystem = sys
  val rowData: Array[Array[Y]] = Array.tabulate(rows, cols) { (r, c) =>
    create(r, c)
  }
  override val length: Int = rows * cols // no recalc...

  @inline final def act(te: Tick, r: Int, c: Int): Double = rowData(r)(c).act
  @inline def apply(r: Int, c: Int): Y = rowData(r)(c)
  initKernel(this)
  override def equals(other: Any): Boolean = other match {
    case a: AnyRef => this eq a
    case default   => false
  }
  override def toString(): String = s"YGrid[rows= $rows, cols= $cols, tau= $tau]"
}
