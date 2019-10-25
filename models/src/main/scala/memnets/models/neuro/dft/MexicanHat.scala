package memnets.models.neuro.dft

import breeze.linalg._
import memnets.model._
import memnets.utils.{BeanSupport, DirtySupport}

class MexicanHat(val halfLength: Int = 32)(implicit sys: DynamicSystem)
    extends BeanSupport
    with DirtySupport
    with Logging {
  val range: DenseVector[Double] = -halfLength :: halfLength
  def kernel: DenseVector[Double] = _kernel
  val c_exc = Param("c_exc", max = 30.0, init = 10.0)
  val c_inh = Param("c_inh", max = 50.0, init = 25.0)
  val sig_exc = Param(GREEK.SIGMA + "_exc", max = 10.0, init = 2.0)
  val sig_inh = Param(GREEK.SIGMA + "_inh", max = 20.0, init = 10.0)
  private val _kernel = DenseVector.zeros[Double](range.length)

  private def updateKernel(): Unit = {
    logger.trace("update kernel")
    _kernel := (gaussNorm(range, sigma = sig_exc.w) *:* c_exc.w) -:- (gaussNorm(range, sigma = sig_inh.w) *:* c_inh.w)
    dirty = true
    dirty = false
  }
  c_exc ==> { d =>
    updateKernel()
  }
  c_inh ==> { d =>
    updateKernel()
  }
  sig_exc ==> { d =>
    updateKernel()
  }
  sig_inh ==> { d =>
    updateKernel()
  }
  updateKernel()
}
