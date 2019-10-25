package memnets.model.impl

import memnets.model.{DynamicSystem, Lambda}

/** NOTE : assumes output is purely function of inputs, not current x */
private[model] final class LambdaImpl(system: DynamicSystem, g: Double => Double) extends Lambda {
  def dxF(out: Array[Float], x: Array[Float], f: Int => Float): Unit = {
    val len = out.length
    var i = 0
    while (i < len) {
      out(i) = g(f(i)).asInstanceOf[Float]
      i += 1
    }
  }
  def dx(out: Array[Double], x: Array[Double], f: Int => Double): Unit = {
    val len = out.length
    var i = 0
    while (i < len) {
      out(i) = g(f(i))
      i += 1
    }
  }
}
