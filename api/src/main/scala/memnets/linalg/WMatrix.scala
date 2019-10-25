package memnets.linalg

import memnets.linalg.impl._

object WMatrix {
  import reflect.runtime.universe._
  def apply(numberType: NumberType): WMatrixImpl[_] = numberType match {
    case NumberType.Doubles =>
      apply[Double]()
    case default =>
      apply[Float]()
  }
  def apply[T: TypeTag](initCapacity: Int = 2048): WMatrixImpl[T] = {
    typeOf[T] match {
      case f if f == typeOf[Float] =>
        new WMatrixImpl[Float](initCapacity).asInstanceOf[WMatrixImpl[T]]
      case d if d == typeOf[Double] =>
        new WMatrixImpl[Double](initCapacity).asInstanceOf[WMatrixImpl[T]]
      case default => ???
    }
  }
}

/** OO Sparse Matrix.  strives for balance between memory/compute/viz/features */
trait WMatrix {
  type T = W

  /** NOTE: inefficient default impl */
  def apply(src: Int, tgt: Int): Double = {
    val opt = outEdges(src).find(w => w.tgt == tgt).map(_.w)
    if (opt.isDefined) opt.get else 0.0
  }

  /** weights = 1.0 by default */
  def create(src: Int, tgt: Int, unique: Boolean = false): T

  /** NOTE: default impl not very efficient */
  def findW(src: Int, tgt: Int): Option[W] = weights.find(w => w.src == src && w.tgt == tgt)
  def inEdges(tgt: Int): Iterator[T] = weights.iterator.filter(e => e.tgt == tgt) // NOT optimized
  def outEdges(src: Int): Iterator[T] = weights.iterator.filter(e => e.src == src) // NOT optimized
  def remove(e: W): Unit
  def sortWeights(variableCount: Int): Unit
  def weights: IndexedSeq[T]
  def numberType: NumberType

  /** NOTE: does not check existing entries.... */
  def update(src: Int, tgt: Int, w: Double): Unit = {
    val weight = create(src, tgt)
    weight.w = w
  }
}
