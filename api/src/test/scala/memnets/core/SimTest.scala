package memnets.core

import breeze.numerics.sigmoid
import memnets.core.impl._
import memnets.linalg.NumberType
import memnets.model._
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class SimTest extends JUnitSuite with MustMatchers with Logging {
  import NumberType._
  // used by builder
  import Math._

  import OdeMethod._
  val ONE_MINUTE = 3600
  var steps = 50 * ONE_MINUTE // 50min @ 60fps
  val tMod: Int = steps / 10
  var doDoublesTests = true
  var tol = 1e-9
  var tolF = 1e-4

  @Test def ode23: Unit = { error(tolF, numberType = Floats, tau = 25) } // breaks at tau = 24
  @Test def ode45: Unit = { error(tol, numberType = Doubles, tau = 10) } // breaks at tau = 9

  @Test def ode23Layer: Unit = { errorLayer(tolF, numberType = Floats) }
  @Test def ode45Layer: Unit = { errorLayer(tol, numberType = Doubles, tau = 10) } // breaks at tau = 9
  @Test def ode45Layer2: Unit = { errorLayer(tolF, numberType = Doubles, methodOpt = Some(Ode23)) } // breaks at tau = 9

  @Test def userFuncs: Unit = runTests {
    ModelBuilder("userFunc") { b =>
      import b._

      val x1 = Y("x1", tau = 10.0)
      val x2 = Y("x2", tau = 1.0, decay = -1.0)
      x2.f("cos", x1) { t =>
        cos(x1)
      }
      Trial(time = 5000).onReset = { x1.update(0.5) }
      //    Step(y = x1, on = 50, dur = 1000000, scale = 0.5)

      validator = te => {
        logr.debug("validator")
        assert(x2.act === cos(x1.act) +- tolF)
      }
    }
  }
  @Test def ic: Unit = runTests {
    ModelBuilder("ic") { b =>
      import b._
      val x1 = Y("x1", tau = 10.0, decay = 0.0)
      Trial(time = 5000)
      Step(y = x1, on = 0, scale = 2.34)
      validator = te => {
        logr.debug("validator")
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def icOnReset: Unit = runTests {
    ModelBuilder("icReset") { b =>
      import b._
      val x1 = Y("x1", tau = 10.0, decay = 0.0)
      Trial(time = 5000).onReset = { x1.update(2.34) }
      validator = te => {
        logr.debug("validator")
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def inStep: Unit = runTests {
    ModelBuilder("inStep") { b =>
      import b._
      val x1 = Y("x1", tau = 10.0, decay = -1.0)
      Step(y = x1, on = 5, dur = -1, scale = 2.34)
      validator = te => {
        logr.debug("validator")
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def customOp: Unit = runTests {
    ModelBuilder("customOp") { b =>
      import b._
      val x1 = Y("x1", tau = 10.0, decay = -1.0)
      x1.out = d => { sigmoid(d) }
      Step(y = x1, on = 5, dur = -1, scale = 1000.0)
      validator = te => {
        logr.debug("validator")
        assert(x1.act === 1.0 +- tolF)
      }
    }
  }

  @Test def icLayer: Unit = runTests {
    ModelBuilder("icLayer") { b =>
      import b._
      val lay = Layer(n = 8, "x1", tau = 10.0, decay = 0.0)
      val x1 = lay.y(1)
      Trial(time = 5000)
      Step(y = x1, on = 0, scale = 2.34)
      validator = te => {
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def icLayerOnReset: Unit = runTests {
    ModelBuilder("icLayerOnReset") { b =>
      import b._
      val lay = Layer(n = 8, "x1", tau = 10.0, decay = 0.0)
      val x1 = lay.y(1)
      Trial(time = 5000).onReset = { x1.update(2.34) }
      validator = te => {
        logr.debug("validator")
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def inLayerStep: Unit = runTests {
    ModelBuilder("inLayerStep") { b =>
      import b._
      val lay = Layer(n = 8, "x1", tau = 10.0, decay = -1.0)
      val x1 = lay.y(1)
      Trial(time = 5000)
      Step(y = x1, on = 10, dur = 10000, scale = 2.34)
      validator = te => {
        assert(x1.act === 2.34 +- tolF)
      }
    }
  }
  @Test def matMul = runTests {
    ModelBuilder("matMul") { b =>
      import b._
      val input = Dense(name = "input", n = 8)
      val out = Dense(name = "out", n = 4)
      val a = input -:> out
      a(0, 0) = 1.0
      a(1, 0) = 0.5
      a(1, 7) = 1.0
      a(3, 7) = 1.0
      Trial()
      Step(y = input.y(0), on = 1, dur = steps, scale = 5.0)
      Step(y = input.y(input.length - 1), on = 1, dur = steps, scale = 3.0)
      validator = te => {
        logr.debug("validator")
        logr.debug(input.toString)
        input(0) must equal(5.0 +- 1e-5)
        input(input.length - 1) must equal(3.0 +- 1e-5)
        logr.debug(out.toString)
        out(0) must equal(5.0 +- 1e-5)
        out(1) must equal(5.5 +- 1e-5)
        out(3) must equal(3.0 +- 1e-5)
      }
    }
  }
  @Test def sparse = runTests {
    ModelBuilder("sparse") { b =>
      import b._
      val input = Layer(name = "input", n = 8, decay = -1.0, tau = 10.0)
      val out = Layer(name = "out", n = 4, decay = -1.0)
      val a = input -%> out

      // mix up order to verify sort
      a(1, 1) = 1.0
      a(0, 0) = 1.0
      a(2, 7) = 2.0
      a(2, 1) = 2.0
      a(1, 0) = 0.5

      // 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      // 0.5, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      // 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0
      // 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0

      Trial()
      Step(y = input.y(0), on = 1, dur = steps, scale = 5.0)
      Step(y = input.y(1), on = 1, dur = steps, scale = 3.0)
      Step(y = input.y(7), on = 1, dur = steps, scale = 2.0)
      validator = te => {
        logr.debug("validator")
        logr.debug(input.toString)
        input(0) must equal(5.0 +- 1e-5)
        input(1) must equal(3.0 +- 1e-5)
        input(7) must equal(2.0 +- 1e-5)
        logr.debug(out.toString)
        out(0) must equal(5.0 +- 1e-5)
        out(1) must equal(5.5 +- 1e-5)
        out(2) must equal(10.0 +- 1e-4)
      }
    }
  }
  @Test def dot = runTests {
    ModelBuilder("dot") { b =>
      import b._
      val input = Layer(n = 8, decay = -1.0, tau = 10.0)
      val out = Y(decay = -1.0)
      val dot = input -:> out
      dot(0) = 1.0
      dot(1) = 0.5
      Step(y = input.y(0), on = 1, dur = steps, scale = 1.0)
      Step(y = input.y(1), on = 1, dur = steps, scale = 3.0)
      Step(y = input.y(2), on = 1, dur = steps, scale = 20.0) // should be ignored
      validator = te => {
        logr.debug("validator")
        logr.debug(input.toString)
        input(0) must equal(1.0 +- 1e-5)
        input(1) must equal(3.0 +- 1e-5)
        logr.debug(out.toString)
        out.act must equal(2.5 +- 1e-5)
      }
    }
  }
  @Test def softmax = runTests {
    ModelBuilder("softmax") { b =>
      import b._
      val input = Input(n = 8)
      input(0) = 4.0
      input(4) = 4.0
      val softMax = SoftMax(input = input, name = "softmax")
      validator = te => {
        logr.debug("validator")
        logr.debug(softMax.toString)
        input(0) must equal(4.0 +- 1e-5)
        input(1) must equal(0.0 +- 1e-5)
        input(4) must equal(4.0 +- 1e-5)
        softMax.sum must equal(1.0 +- 1e-4)
        softMax(0) must equal(0.5 +- 3e-2)
        softMax(1) must equal(0.01 +- 1e-2)
        softMax(4) must equal(0.5 +- 3e-2)
      }
    }
  }
  @Test def lamdbaLayer = runTests {
    ModelBuilder("lambdalayer") { b =>
      import b._
      val input = Input(n = 8)
      input(0) = 4.0
      input(4) = 4.0
      val lambda = LambdaLayer(n = 8, name = "tanh") { tanh(_) }
      input --> lambda
      validator = te => {
        logr.debug("valitdator")
        input(0) must equal(4.0 +- 1e-5)
        input(1) must equal(0.0 +- 1e-5)
        input(4) must equal(4.0 +- 1e-5)
        lambda(0) must equal(tanh(4.0) +- 3e-2)
        lambda(1) must equal(tanh(0.0) +- 1e-2)
        lambda(4) must equal(tanh(4.0) +- 3e-2)
      }
    }
  }
  @Test def lamdaLayerCos = runTests {
    ModelBuilder("lambdaLayerCos") { b =>
      import b._
      val input = Input(n = 8)
      input.random()
      input(0) = 4.0
      input(4) = 4.0
      val f = Math.cos(_)
      val lambda = LambdaLayer(n = 8, f = f)
      logr.debug("lamLay: " + lambda)
      input --> lambda
      validator = te => {
        logr.debug("validator")
        input(0) must equal(4.0 +- 1e-5)
        input(4) must equal(4.0 +- 1e-5)
        lambda(0) must equal(f(input(0)) +- 1e-5)
        lambda(1) must equal(f(input(1)) +- 1e-5)
        lambda(2) must equal(f(input(2)) +- 1e-5)
      }
    }
  }
  @Test def customIterate: Unit = {
    val cfg = ModelConfig(
      numberType = NumberType.Floats,
      customProvider = (sys, cfg) => {
        val sim = new SimImplBase(sys, cfg.paraThreshold) {
          override protected def custom = new IterMethod("EulerCust") {
            val _k1 = zerosStack
            val iterBuffers = Array(_k1)
            val _k1Data = data(_k1)
            def k1F(id: Int, i: Int) = _k1Data(id)(i)
            def iterate(x: STK, in: SIGS, ns: NoiseSampler) = {
              _isLastDx = true
              dx(_k1, x, in, ns)
              step(x, x, k1F, ns)
            }
          }
        }
        sim.method = cfg.method // must be set b4 sync
        sim.sync()
        sim.iterMethod.name must equal("EulerCust")
        sim
      }
    )
    cfg.method = OdeMethod.Custom

    runSteps(cfg) {
      ModelBuilder("customIterate") { b =>
        import b._
        val x1 = Y("x1")
        val x2 = Y("x2", decay = -1.0)
        x1 --> x2 w = 0.5
        Step(y = x1, on = 0, scale = 5.0)

        validator = te => {
          logr.debug("validator")
          x1.act must equal(5.0 +- 1e-5)
          x2.act must equal(2.5 +- 1e-5)
        }
      }
    }
  }

  protected def error(
      tol: Double,
      numberType: NumberType,
      tau: Double = DynamicSystem.TAU_DEFAULT,
      simSteps: Int = steps): Unit = {

    logger.debug("testing error limits")
    val f = 2.0 * PI / 180.0
    val ph = 0.2
    var osc: Osc = null.asInstanceOf[Osc]
    val builder = ModelBuilder("osc") { b =>
      import b._
      osc = Osc(freq = f)
      Trial().onReset = {
        osc.init(phase = ph)
      }
    }
    val model = builder.build(config(numberType, tau))
    val sim = model.buildSim()
    assert(model.system.numberType === numberType)
    assert(model.system.tau === tau)
    assert(osc.y.tau === tau)
    assert(osc.x.tau === tau)

    val method = if (numberType == Doubles) Ode45 else Ode23
    assert(sim.method === method)

    for (t <- 0 until simSteps) {
      val act = osc.y.act
      assert(act === sin(ph + f * t / tau) +- tol, sim.nowTick)
      sim.step()
      sim.nowTick.forceGpuSyncAll()
      if (t % tMod == 0)
        logger.debug(s"t = $t, osc = $act")
    }
    sim.destroy()
    model.destroy()
  }
  protected def errorLayer(
      tol: Double,
      numberType: NumberType,
      tau: Double = 30.0,
      methodOpt: Option[OdeMethod] = None
  ): Unit = {

    logger.debug("layer_error")
    val f = 2.0 * PI / 180.0
    val ph = 0.2
    var oscs: OscPop = null.asInstanceOf[OscPop]
    val builder = ModelBuilder("osc field") { b =>
      import b._
      oscs = OscPop(size = 8, freq = f)
      Trial().onReset = {
        for (i <- 0 until oscs.size)
          oscs.init(i, phase = ph * i, scale = 1.0)
      }
    }
    val method = methodOpt.getOrElse { if (numberType == Doubles) Ode45 else Ode23 }
    val cfg = config(numberType = numberType, tau = tau)
    cfg.method = method

    val model = builder.build(cfg)
    val sim = model.buildSim()
    assert(model.system.numberType === numberType)
    assert(model.system.tau === tau)
    assert(oscs.y.tau === tau)
    assert(oscs.x.tau === tau)
    assert(sim.method === method)

    for (t <- 0 until steps) {
      var i = 0
      while (i < oscs.size) {
        assert(oscs.y(i) === sin(i * ph + f * t / tau) +- tol, s"t = $t")
        i += 1
      }
      sim.step()
      sim.nowTick.forceGpuSync(oscs.y)
      if (t % ONE_MINUTE == 0) logger.debug(s"t = $t")
    }
    sim.destroy()
    model.destroy()
  }

  protected def config(
      numberType: NumberType,
      tau: Double = DynamicSystem.TAU_DEFAULT
  ) = ModelConfig(numberType, tau)

  protected def runTests(f: ModelBuilder): Unit = {
    logger.debug("testing floats")
    runSteps(config(numberType = NumberType.Floats)) { f }

    if (doDoublesTests) {
      logger.debug("testing doubles")
      runSteps(config(numberType = NumberType.Doubles)) { f }
    }
  }
}
