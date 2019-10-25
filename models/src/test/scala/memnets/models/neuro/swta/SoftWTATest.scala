package memnets.models.neuro.swta

import memnets.core._
import memnets.linalg.NumberType
import memnets.model._
import org.junit._
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class SoftWTATest extends JUnitSuite with MustMatchers with Logging {
  // used by builder
  var steps = 50 * 3600 // 50min @ 60fps

  @Test def swtaGlobalMin: Unit = {
    swtaHelper(useGlobalMin = true)((n: Int, net: DynamicSystem) => new SoftWTA(n = n)(net))
  }
  @Test def swta: Unit = { swtaHelper(useGlobalMin = false)((n: Int, net: DynamicSystem) => new SoftWTA(n = n)(net)) }
  @Test def swtaDoubles: Unit = {
    swtaHelper(useGlobalMin = false, cfg = ModelConfig())((n: Int, net: DynamicSystem) => new SoftWTA(n = n)(net))
  }
  @Test def swtam: Unit = { swtaHelper()((n: Int, net: DynamicSystem) => SoftWTALayer(n = n)(net)) }

  private def swtaHelper(useGlobalMin: Boolean = true, cfg: ModelConfig = ModelConfig(numberType = NumberType.Floats))(
      f: (Int, DynamicSystem) => AbstractSoftWTA[_ <: Yb]) = {

    runSteps(cfg, steps) {

      ModelBuilder("swta") { b =>
        import b._
        if (useGlobalMin) system.sparse.activation = Activation.Relu
        val swta = f(128, system)
        // this version will test constant input
        Trial(steps)
        //   Trial(steps, custom = t => t.resetListener = { swta(0).update(1.0) })
        Step(y = swta(0), on = 1, dur = steps / 2, scale = 1.0) // this tests inputs as well

        validator = te => {
          assert(swta.inhib.act > 0.0)
          assert(swta(0).act > 0.0)
          val alpha: Double = 1.0 + swta.e2eT.getValue
          val det: Double = 1.0 - swta.i2eT.getValue * swta.e2iT.getValue - alpha
          println(det) // should be 0.8
          det must equal(0.8 +- 1e-5)
          val tol = 1e-3
          val detCalc = Math.abs(swta.i2eT.getValue).toDouble / det
          swta(0).act must equal(detCalc +- tol) // Tinh * Beta1    (1.0*5.0)/0.8 = 6.25
          // not working...    import Tolerance._
          for (n <- swta.tail) n.act must equal(0.0 +- tol)
        }
      }
    }
  }
}
