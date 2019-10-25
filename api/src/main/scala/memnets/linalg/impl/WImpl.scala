package memnets.linalg.impl

import memnets.linalg._

private[linalg] final class WImpl(var id: Int, private val coo: WMatrixImpl[_]) extends W {
  def src = coo.getSrc(id)
  def tgt = coo.getTgt(id)
  def w = coo.getWeight(id)
  def w_=(f: Double): this.type = {
    coo.setWeight(id, f)
    this
  }
  override def toString = s"$src-->$tgt $w"
}
