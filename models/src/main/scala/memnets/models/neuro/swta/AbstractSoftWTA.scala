package memnets.models.neuro.swta

import memnets.model._

trait AbstractSoftWTA[T <: Yb] extends IndexedSeq[T] {
  def e2eT: TieType
  def e2iT: TieType
  def i2eT: TieType
  def inhib: Y
}
trait AbstractSoftWTASum[T <: Yb] extends AbstractSoftWTA[T] {
  def sum: Y
  def sum2inh: E
}
abstract class SoftWTASparse extends ElementBase with AbstractSoftWTA[Y] with Layout {
  override def loc = inhib.ui.loc
  override def loc_=(loc: Loc): Unit = { inhib.ui.loc = loc }

  def excites: IndexedSeq[Y]
  def length = excites.length
  def apply(i: Int) = excites(i)

  def layout() = layoutHelper()
  def layoutHelper(
      ctr: Loc = ui.loc,
      onBottom: Boolean = true,
      spacing: Double = (Display.width - 400.0) / size,
      vspacing: Double = 140.0): Unit = {
    inhib.ui.loc = ctr
    val dir = if (onBottom) 1.0 else -1.0
    this.center(ctr.down(dir * vspacing), spacing)
  }
}
