package memnets.model.impl

import memnets.model.{LambdaCalc, Param}

private[model] final class ChainImpl(val ffTie: Param) extends LambdaCalc {
  override def name = "chain"
  def dxF(out: Array[Float], x: Array[Float]): Unit = {
    val ffF: Float = ffTie.w.asInstanceOf[Float]
    var i = 1
    val len = out.length
    while (i < len) {
      out(i) += x(i - 1) * ffF
      i += 1
    }
  }
  def dx(out: Array[Double], x: Array[Double]): Unit = {
    val ff = ffTie.w
    var i = 1
    val len = out.length
    while (i < len) {
      out(i) += x(i - 1) * ff
      i += 1
    }
  }
}

private[model] final class ChainBackwards(val ffTie: Param) extends LambdaCalc {
  def dxF(out: Array[Float], x: Array[Float]): Unit = {
    val ffF: Float = ffTie.w.asInstanceOf[Float]
    var i = out.length - 2
    while (i > 0) {
      out(i) += x(i + 1) * ffF
      i -= 1
    }
  }
  def dx(out: Array[Double], x: Array[Double]): Unit = {
    val ff = ffTie.w
    var i = out.length - 2
    while (i > 0) {
      out(i) += x(i + 1) * ff
      i -= 1
    }
  }
}
