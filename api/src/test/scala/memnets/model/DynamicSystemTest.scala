package memnets.model

import breeze.linalg.DenseMatrix
import memnets.model.Tick.MockTick
import memnets.utils._
import org.junit._
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class DynamicSystemTest extends JUnitSuite with MustMatchers with Logging {
  implicit var sys: DynamicSystem = null
  val tol = 1e-7
  @Before def before: Unit = {
    sys = DynamicSystem()
    assert(sys.variables.length === 0)
    assert(sys.sparse.nodes.isEmpty)
  }

  @Test def create: Unit = {
    val n1 = Y()
    assert(sys.variables.contains(n1))
    assert(n1.id === 0)
    assert(sys.variables(n1.id) === n1)
    assert(n1.layerId === 0)
  }
  @Test def sparse: Unit = {
    assert(sys.variables.size === 0)
    assert(sys.sparse.length === 0)

    val n1 = Y()
    val n2 = Y()

    assert(sys.variables.size === 2)
    assert(sys.sparse.length === 2)

    assert(sys.sparse.ui.gradient.isEmpty)

    sys.sparse.ui.gradient = GradientHints(maxLength = 20)

    assert(sys.sparse.ui.gradient.isDefined)
    assert(sys.sparse.ui.gradient.get.maxLength === 20)
  }
  @Test def variables: Unit = {
    assert(sys.variables.size === 0)
    val n1 = Y()
    assert(sys.variables.size === 1)
    assert(sys.variables.contains(n1))
    val n2 = Y()
    assert(sys.variables.size === 2)
    assert(sys.variables.contains(n1))
    assert(sys.variables.contains(n2))
    assert(n1.id === 0)
    assert(n2.id === 1)
    assert(sys.variables(n1.id) === n1)
    assert(sys.variables(n2.id) === n2)
  }
  @Test def variablesEquals: Unit = {
    val n1 = Y()
    val n2 = Y()
    assert(sys.variables.contains(n1))
    assert(sys.variables.contains(n2))
    assert(n1 != n2)
  }
  @Test def variableName: Unit = {
    val name = "exc0"
    val x1 = Y(name = name)
    assert(name === x1.name)
    assert(name === x1.description)
    assert(name === x1.toString)
  }
  @Test def variableProperties: Unit = {
    val key = "TEST"
    val x1 = Y()
    x1(key) = 2.0
    assert(x1.get(key).isDefined)
    assert(x1.get[Float](key).get === 2.0)
    assert(x1[Float](key) === 2.0)

    val x2 = Y()
    x2(key) = 3.0
    assert(x1[Float](key) === 2.0)
    assert(x2.get(key).isDefined === true)
    assert(x2.get[Float](key).get === 3.0)
    assert(x2[Float](key) === 3.0)

    val opt = x2.update("test", 2.0f)
    assert(opt.isEmpty)

    val opt2 = x2.update("test", 3.0f)
    assert(opt2.get === 2.0)
  }
  @Test def variableConfig: Unit = {
    val x1 = Y()
    assert(x1.threshold === 0.0f)
    x1.threshold = 0.5
    assert(x1.threshold === 0.5f)

    x1.threshold = 1.5f
    assert(x1.threshold === 1.5f)
  }
  @Test def variableFlag: Unit = {
    import Config.SHOW_TEXT
    val x1 = Y()
    assert(x1.hasFlag(SHOW_TEXT) === false)
    assert(x1.ui.showText === false)
    assert(x1.hasFlag(SHOW_TEXT) === false)
    x1.ui.showText = true
    assert(x1.ui.showText === true)
    assert(x1.hasFlag(SHOW_TEXT) === true)

    val x2 = Y()
    assert(x2.ui.showText === false)
    x2.ui.showText = true
    assert(x2.ui.showText === true)

    x1.ui.showText = false
    assert(x1.ui.showText === false)
    assert(x2.ui.showText === true)

  }
  @Test def weight: Unit = {
    weightHelper(false)
  }
  @Test def weightOptimize: Unit = {
    weightHelper(true)
  }
  protected def weightHelper(optimize: Boolean): Unit = {
    val src = Y("x")
    val tgt = Y("y")
    assert(sys.variables.contains(src))
    assert(sys.variables.contains(tgt))
    val e = src --> tgt
    if (optimize) sys.compact()
    assert(src.id === e.src)
    assert(tgt.id === e.tgt)
    assert(e.w === 1.0) // default is to connect
    var outEdges = sys.sparse.outEdges(src).toList
    assert(outEdges.size === 1)
    assert(src.outs.size == 1)
    var head = outEdges.head
    assert(src.id === head.src)
    assert(tgt.id === head.tgt)
    assert(src.outs.headOption.get == head)
    assert(sys.sparse.inEdges(src).isEmpty)
    assert(src.ins.isEmpty)

    var inEdges = sys.sparse.inEdges(tgt).toList
    assert(inEdges.size === 1)
    assert(tgt.ins.size === 1)
    head = inEdges.head
    assert(tgt.id === head.tgt)
    assert(src.id === head.src)
    assert(tgt.ins.headOption.get === head)
    assert(sys.sparse.outEdges(tgt).isEmpty)
    assert(tgt.outs.isEmpty)

    val w = 0.6
    e.w = w
    assert(e.w === w +- tol)

    // loops
    val e2 = src --> src
    assert(src.id === e2.src)
    assert(src.id === e2.tgt)
    assert(e2.w === -1.0) // default loop = -1.0
    outEdges = sys.sparse.outEdges(src).toList
    assert(outEdges.size === 2)
    assert(src.outs.size == 2)

    val noLoops = src.outsNoLoops.toList
    assert(noLoops.size === 1)
    assert(noLoops.contains(e))
  }
  @Test def paramOnChange: Unit = {
    val tie = Param("tie1", 2.0, 0.4)
    tie.func = d => d * d

    var count = 0
    var lastValue = 0.0
    val sub = tie ==> { d =>
      lastValue = d
      count += 1
    }
    assert(count === 1)
    assert(lastValue === 0.16 +- tol)

    tie.value = 0.5
    assert(count === 2)
    assert(lastValue === 0.25 +- tol)

    sub.cancel()

    tie.value = 0.6
    assert(count === 2)
    assert(lastValue === 0.25 +- tol)
  }
  @Test def paramFunc: Unit = {
    val tie = Param("tie1", 2.0, 0.4)
    assert(tie.value === 0.4 +- tol)
    assert(tie.w === 0.4 +- tol)

    tie.func = d => d * d
    assert(tie.value === 0.4 +- tol)
    assert(tie.w === 0.16 +- tol)

    tie.value = 0.8
    assert(tie.value === 0.8 +- tol)
    assert(tie.w === 0.64 +- tol)
  }
  @Test def param: Unit = {
    val src = Y("x")
    val tgt = Y("y")
    val tgt2 = Y("z")
    val e = src --> tgt
    assert(e.w === 1.0)
    assert(e.tie === None)
    val e2 = src --> tgt2

    val tie = Param("tie1", 2.0, 0.4)
    assert(tie.tied.size === 0)
    e.tie = tie
    assert(e.tie.get === tie)
    assert(e2.tie === None)
    assert(e.w === tie.getValue +- tol)
    assert(tie.tied.size === 1)
    assert(tie.tied.contains(e))

    val w = 0.45
    tie.value = w
    assert(e.w === w +- tol)
    assert(e2.w === 1.0 +- tol)

    e.tie = None
    assert(e.tie === None)
    val w2 = 0.7
    e.w = w2
    assert(e.w === w2 +- tol)
    assert(tie.value === w +- tol)
    assert(tie.tied.size === 0)
  }
  @Test def decay: Unit = {
    val n = Y("x")
    assert(sys.sparse.inEdges(n).size === 0)
    assert(sys.sparse.outEdges(n).size === 0)
    val w = -0.2
    n --> n w = w
    sys.sparse.loop(n).get.w must equal(w +- tol)
    assert(n.decay.isDefined)
    n.decay.get.w must equal(w +- tol)
    assert(sys.sparse.outEdges(n).size === 1)
    assert(sys.sparse.inEdges(n).size === 1)
  }
  @Test def dot: Unit = {
    val n = 10
    val xarray = Layer(n = 10)
    val out = Y("out")
    val edge = xarray -:> out
    assert(xarray === edge.src)
    assert(out === edge.tgt)
    val w = edge.toDenseVector()
    assert(w.length === n)
  }

  /** don't like must matchers, but leaving this guy  */
  @Test def layer: Unit = {
    val n = 8
    var lay = Layer(n = n)
    lay.length must equal(n)
    sys.variables.size must equal(0)
    lay.id must equal(1)
    lay.y(0).id must equal(0)
    lay.y(0).layerId must equal(lay.id)
    lay.y(1).id must equal(1)
    lay.y(0) must equal(lay.y(0))
    lay.decay must equal(0.0)

    lay = Layer(n = n, name = "test", decay = -0.5, tau = 90.0, act = Activation.Relu)
    lay.length must equal(n)
    lay.id must equal(2)
    lay.y(0).id must equal(0)
    lay.y(1).id must equal(1)
    lay.y(0).layerId must equal(lay.id)
    lay.name must equal("test")
    lay.decay must equal(-0.5)
    lay.tau must equal(90.0)
    lay.activation must equal(Activation.Relu)

  }
  @Test def layerLoc: Unit = {
    val lay = Layer(n = 8)
    lay.ui.loc = Loc(0, 100)

    val y0 = lay.y(0)
    assert(y0.ui.loc.y === lay.ui.loc.y)

    val y1 = lay.y(1)
    assert(y1.ui.loc.y === lay.ui.loc.y)
  }
  @Test def oscPhase: Unit = {
    val osc = Osc()
    sys.now = new MockTick(sys)

    def helper(ph: Double): Unit = {
      logger.debug(s"phase: $ph, deg: ${Math.toDegrees(ph)}")
      osc.init(phase = ph, scale = 4.2)
      val ph2 = if (ph <= 2.0 * Math.PI) ph else ph - 2.0 * Math.PI
      assert(osc.phase === ph2 +- tol)
    }
    helper(Math.PI / 2.0)
    helper(Math.PI)
    helper(1.5 * Math.PI)
    helper(2.0 * Math.PI)

    helper(Math.PI / 4.0)
    helper(Math.PI / 6.0)
    helper(Math.PI / 8.0)
    helper(2.5 * Math.PI)
  }
  @Test def oscToMatrix: Unit = {
    val f = 2.0
    val d = -0.01
    val osc = Osc(freq = f, damping = d)
    val m = sys.sparse.toMatrix
    m.toString()
    assert(m(0, 0) === 0.0 +- tol)
    assert(m(0, 1) === 1.0 +- tol)
    assert(m(1, 0) === -f * f +- tol)
    assert(m(1, 1) === d +- tol)
  }
  @Test def fromAndToMatrix: Unit = {
    val in = DenseMatrix((0.0, 1.0), (2.0, 0.05))
    val variables = sys.sparse.fromMatrix(in)
    assert(variables.length === 2)
    assert(sys.variables.length === 2)

    val x0 = variables(0)
    val x1 = variables(1)
    assert(x0.decay.isEmpty)
    assert(x1.decay.get.w === 0.05 +- tol)

    assert(x1.outsNoLoops.find(_.tgt === x0.id).get.w === 1.0 +- tol)
    assert(x1.outsNoLoops.size === 1)

    assert(x0.outsNoLoops.find(_.tgt === x1.id).get.w === 2.0 +- tol)
    assert(x0.outsNoLoops.size === 1)

    val m = sys.sparse.toMatrix
    m.toString()
    assert(m.size === 4)
    assert(m(0, 0) === in(0, 0) +- tol)
    assert(m(0, 1) === in(0, 1) +- tol)
    assert(m(1, 0) === in(1, 0) +- tol)
    assert(m(1, 1) === in(1, 1) +- tol)
  }
  @Test def fromMatrixSWTA: Unit = {
    val file = "swta.csv".streamToTempFile().getAbsolutePath
    val matrix = breeze.linalg.csvread(file = file)
    val variables = sys.sparse.fromMatrix(matrix)

    assert(variables.length === 6)
    val inhib = sys.variables(0)
    val excites = sys.variables.tail
    assert(excites.length === 5)

    assert(inhib.decay.get.w === -1.0)
    for (e <- excites) {
      assert(e.decay.get.w === 0.2)
      val i2e = inhib.outs.find(_.tgt == e.id)
      assert(i2e.get.w === -5.0)

      assert(e.outsNoLoops.size === 1)
      val e2i = e.outsNoLoops.find(_.tgt == inhib.id)
      assert(e2i.get.w === 0.2)
    }

    assert(inhib.outsNoLoops.size === 5)
    /*
        val m = sys.sparse.toMatrix
        m.toString()
        assert(m.size === 4)
        assert(m(0,0) === in(0,0) +- tol)
        assert(m(0,1) === in(0,1) +- tol)
        assert(m(1,0) === in(1,0) +- tol)
        assert(m(1,1) === in(1,1) +- tol)

   */
  }
  @Test def userfunc: Unit = {
    val n1 = Y()
    val n2 = Y()
    val n3 = Y()
    assert(n1.functions.isEmpty)
    val mockTick = new MockTick(sys)
    sys.now = mockTick

    val desc = "n2 * n3"
    val uf = n1.f(desc, n2, n3) { t =>
      n2 * n3
    }
    assert(n1.functions.head === uf)
    assert(uf.inputs.contains(n2))
    assert(uf.inputs.contains(n3))
    assert(uf.description === desc)
    n2.update(3.0)
    n3.update(4.0)
    assert(uf.eval(sys.now) === 12.0)

    mockTick.clear()
    n2.update(2.0)
    n3.update(5.0)
    assert(uf.eval(sys.now) === 10.0)

    val uf2 = n3.f(desc) { t =>
      n3 * n3
    }
    mockTick.clear()
    n3.update(6.0)
    assert(uf2.eval(sys.now) === 36.0)
    assert(uf.inputs.contains(n3))
  }
  @Test def display: Unit = {
    var count = 0
    var lastRes = Display.resolution
    val sub = Display.onChange { res =>
      count += 1
      lastRes = res
    }
    assert(count === 1) // init called
    def helper(res: Resolution, test: Resolution): Unit = {
      assert(res.width === test.width)
      assert(res.height === test.height)
      assert(lastRes === test)
    }

    Display.resolution = HD720
    helper(Display.resolution, HD720)
    assert(count === 1) // no change

    Display.resolution = HD1080
    helper(Display.resolution, HD1080)
    assert(count === 2)

    Display.resolution = HD720
    helper(Display.resolution, HD720)
    assert(count === 3)

    sub.cancel()
    Display.resolution = HD1080
    assert(count === 3)
  }
}
