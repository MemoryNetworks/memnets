package memnets.linalg

import breeze.linalg.operators.OpMulMatrix

trait InPlaceImpl3P[V, V2, V3] {
  def apply(v: V, v2: V2, v3: V3, scale: Double = 1.0): Unit
}

trait CooMatrixLike[T] extends CooMatrixLikeOps[CooMatrixLike[T]] {
  def repr: CooMatrixLike[T] = this
  def rowData: Array[Int]
  def colData: Array[Int]
  def wData: Array[T]
  def activeSize: Int
  def sortType: COOSortType
  def sorted: Boolean
}
trait CooMatrixLikeOps[+This] extends Any {
  def repr: This

  final def *[TT >: This, B, R](b: B, out: R)(implicit op: OpMulMatrix.InPlaceImpl3[TT, B, R]) = {
    op(repr, b, out)
  }
  final def *^[TT >: This, B, R](b: B, out: R, scale: Double = 1.0)(implicit op: InPlaceImpl3P[TT, B, R]) = {
    op(repr, b, out, scale)
  }
}

object CooMatrixLike {
  implicit object implOpMulMatrix_F extends OpMulMatrix.InPlaceImpl3[CooMatrixLike[Float], Array[Float], Array[Float]] {
    def apply(matrix: CooMatrixLike[Float], x: Array[Float], out: Array[Float]) = {
      val srcs = matrix.colData
      val tgts = matrix.rowData
      val w = matrix.wData
      val end = matrix.activeSize
      var i = 0
      while (i < end) {
        val act = x(srcs(i))
        if (act != 0.0f) out(tgts(i)) += act * w(i)
        i += 1
      }
    }
  }
  implicit object implOpMulMatrix_D
      extends OpMulMatrix.InPlaceImpl3[CooMatrixLike[Double], Array[Double], Array[Double]] {
    def apply(matrix: CooMatrixLike[Double], x: Array[Double], out: Array[Double]) = {
      val srcs = matrix.colData
      val tgts = matrix.rowData
      val w = matrix.wData
      val end = matrix.activeSize
      var i = 0
      while (i < end) {
        val act = x(srcs(i))
        if (act != 0.0) out(tgts(i)) += act * w(i)
        i += 1
      }
    }
  }

  implicit object implOpMulMatrixTranspose_F extends InPlaceImpl3P[CooMatrixLike[Float], Array[Float], Array[Float]] {
    def apply(matrix: CooMatrixLike[Float], x: Array[Float], out: Array[Float], scale: Double) = {
      val scaleF: Float = scale.asInstanceOf[Float]
      val tgts = matrix.colData
      val srcs = matrix.rowData
      val w = matrix.wData
      val end = matrix.activeSize
      var i = 0
      while (i < end) {
        val act = x(srcs(i))
        if (act != 0.0f) out(tgts(i)) += act * w(i) * scaleF
        i += 1
      }
    }
  }
  implicit object implOpMulMatrixTranspose_D
      extends InPlaceImpl3P[CooMatrixLike[Double], Array[Double], Array[Double]] {
    def apply(matrix: CooMatrixLike[Double], x: Array[Double], out: Array[Double], scale: Double) = {
      val tgts = matrix.colData
      val srcs = matrix.rowData
      val w = matrix.wData
      val end = matrix.activeSize
      var i = 0
      while (i < end) {
        val act = x(srcs(i))
        if (act != 0.0) out(tgts(i)) += act * w(i) * scale
        i += 1
      }
    }
  }
}
