package memnets.models

import java.lang.Math.random

import breeze.stats.distributions._
import memnets.core.SciBranch._
import memnets.core._
import memnets.linalg.NumberType
import memnets.ml.PageRank
import memnets.model.Activation._
import memnets.model._
import memnets.models.biology._
import memnets.models.chaos._
import memnets.models.control._
import memnets.models.neuro.spiking._
import memnets.models.neuro.swta._
import memnets.models.neuro.swta.fsm._
import memnets.ui._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * core library curated by MemNets.
 *
 * if you'd like to contribute a model, please make modifications to CommunityLibrary
 */
object StandardLibrary extends Library {
  import CommonTopics._
  // tags
  val sWTATags = Seq("sWTA", "nonlinear", "relu", "competitive learning", "Douglas", "Maass")

  val swta = ModelBuilder(
    "Soft Winner-Take-All",
    Neuroscience,
    tags = sWTATags,
    library = this,
    topic = swtaTopic,
    hints = ModelHints(tau = 5.0)
  ) { model =>
    import model._

    val size = sizeHint(default = 5, min = 2)
    val swta = new SoftWTA(n = size)
    val chainTie = Param("chain", max = 1.0, init = 0.2)
    chain(swta, chainTie, KernelType.NoWrap)

    Trial(name = "A not B", time = 20 s).onInit = { chainTie.value = 0.0 }
    Step(y = swta(0), on = 1 s, dur = 10 s, scale = 3.0, desc = "signal for A").descOff =
      "signal A off releases block on B"
    Step(y = swta(1), on = 5 s, dur = 10 s, scale = 3.2, desc = "stronger signal for B blocked").descOff =
      "signal B off but remains active"

    Trial(name = "competition", time = 15 s).onInit = { chainTie.value = 0.0 }
    Step(y = swta(0), on = 3 s, dur = 8 s, scale = 3.0)
    Step(y = swta(1), on = 3 s, dur = 5 s, scale = 3.2, desc = "stronger signal for B beats A").descOff =
      "signal B off release block on A"

    Trial(name = "sequence", time = 2 m).onInit = { chainTie.value = 0.0 }
    Sequence(swta)(on = 100, dur = 100, scale = 3.0, delay = 130)

    Trial(name = "chain", time = 1 m).onInit = { chainTie.value = 0.2 }
    Step(y = swta(0), on = 3 s, dur = 2 s, scale = 3.0)

    Trial(name = "chain no inputs").onInit = { chainTie.value = 0.2 }
  }
  val swtaDist = ModelBuilder(
    "sWTA Distributed",
    Neuroscience,
    tags = sWTATags,
    library = this,
    desc =
      """This sWTA variation uses multiple distributed sWTAs, each with their own local inhibition linked to a global inhibition.
        |""".stripMargin,
    topic = swtaTopic
  ) { model =>
    import model._

    val size = sizeHint(default = 3)
    val swtaDist = new SoftWTADist(count = 5, n = size)
    Sequence(swtaDist.swtas.map(_.excites.head))(on = 20, dur = 100, delay = 350, scale = 3.0)
  }

  val swtaWithSum = ModelBuilder(
    "sWTA w/ Sum",
    Neuroscience,
    tags = sWTATags,
    library = this,
    desc = """This sWTA variation adds a summation variable that all excites converge on instead of inhib.
             |
             |The summation variable then feeds into inhibition.
             |
             |Functionally, this model is roughly equivalent to the standard sWTA if tau = 1
             |""".stripMargin,
    topic = swtaTopic
  ) { model =>
    import model._

    val swta = new SoftWTASum(n = 4)
    Step(y = swta(0), on = 20, dur = 40, scale = 2.0)

    skin = Skin { x =>
      x.backImageOn = true
      x.backImage = SkinImage.FIVE
    }
  }

  val swtaLayer = ModelBuilder(
    "sWTA Layer",
    Neuroscience,
    tags = sWTATags ++ Seq("feed-forward"),
    library = this,
    desc = """This model uses the Layer API.
             |
             |It also chains together the excitation layer with an adjustable Param "chain".
             |""".stripMargin,
    topic = swtaTopic
  ) { model =>
    import model._

    val size = sizeHint(default = 2048)

    val swta = SoftWTALayer(n = size)
    swta.inhib.ui.color = Colorf.INHIB

    val ff = Param("chain", max = 1.2, init = 0.8)
    swta.excites.chain(ff)

    val probe = swta.length / 4

    val poolA = Y("PoolA", decay = -1.0, act = Relu)
    val dottA = swta.excites --> poolA
    dottA.srcRange.modify(probe + 4, probe + 8)

    val poolB = Y("PoolB", decay = -1.0, act = Relu)
    val dottB = swta.excites --> poolB
    dottB.srcRange.modify(probe + 12, probe + 16)

    poolA.ui.loc = Loc().up(140).left(200)
    poolB.ui.loc = poolA.ui.loc.right(60)

    system.onTick = { te =>
      if (te.t % 10 == 0) {
        swta.excites.topK(min = 0.0)
      }
    }
    track(poolA, poolB)

    system.elements += new Sliding(swta.excites) {
      loc = Loc().down(150)
      // bigger width (even after scaled down) -> better tracking
      hints = GridHints(width = 2000, height = 200, scaleX = 0.5)
    }
    skin = Skin { x =>
      x.backColor = Colorf.LIGHTBLACK
    }
    Step(y = swta(probe), on = 20, dur = 40, scale = 2.0)
  }

  val dft = ModelBuilder(
    "Dynamic Field Theory",
    Neuroscience,
    tags = List("convolution", "Schöener", "Spencer"),
    library = this,
    hints = ModelHints(ode = OdeMethod.Euler)
  ) { model =>
    import memnets.models.neuro.dft._
    import model._

    val size = sizeHint(default = 128)
    val dft = new DFTLayer(n = size)
    import dft._
    import dft.mexicanHat.kernel

    // just here to see kernel
    val kernel_ui = InputP(n = kernel.length, name = "kernel", numericType = VariableType.Continuous)
    kernel_ui := kernel
    mexicanHat onDirty = { kernel_ui := kernel }
    kernel_ui.ui.scale = 1.5
    kernel_ui.ui.gradient = Colorf.DEEPPINK
    kernel_ui.ui.plot.width = 400

    out_u.ui.scale = field_u.ui.scale.get
    field_u.ui.gradient = Colorf.DODGERBLUE
    out_u.ui.gradient = Colorf.INDIANRED
    inhib.ui.color = Colorf.INHIB

    Plot(field_u, out_u)
    kernel_ui.ui.plot.showZeroLine = true
    //Plot(Seq(kernel_ui), showZeroLine = true)

    system.onLayout = {
      field_u.ui.loc = Loc()
      kernel_ui.ui.loc = field_u.ui.loc.down(200)
      inhib.ui.loc = kernel_ui.ui.loc.right(300)
    }
    val A = field_u.y(n / 2)
    val B = field_u.y(n / 2 + 6)
    tracked ++= Seq(A, inhib)

    Trial(name = "A not B", time = 1 m)
    Step(y = A, on = 5 s, dur = 50 s, scale = 1.4, desc = "input to A held for 50s")
    Step(y = B, on = 10 s, dur = 5 s, scale = 1.1, desc = "weak input to B for 5s")
    Step(y = B, on = 20 s, dur = 5 s, scale = 1.4, desc = "equal input to B for 5s")
    Step(y = B, on = 30 s, dur = 5 s, scale = 2.4, desc = "stronger input to B for 5s")
  }

  val hybrid = ModelBuilder(
    "Hybrid Dynamic System",
    Engineering,
    tags = sWTATags ++ Seq("hybrid", "discrete"),
    library = this,
    topic = Topic(
      """A hybrid system is a dynamical system that exhibits both continuous and discrete dynamic behavior – a system that can both flow (described by a differential equation) and jump (described by a state machine or automaton).
        |
        |Often, the term "hybrid dynamical system" is used, to distinguish over hybrid systems such as those that combine neural nets and fuzzy logic, or electrical and mechanical drivelines.
        |
        |A hybrid system has the benefit of encompassing a larger class of systems within its structure, allowing for more flexibility in modeling dynamic phenomena.
        |
        |In general, the state of a hybrid system is defined by the values of the continuous variables and a discrete mode.
        |
        |The state changes either continuously, according to a flow condition, or discretely according to a control graph.
        |
        |Continuous flow is permitted as long as so-called invariants hold, while discrete transitions can occur as soon as given jump conditions are satisfied.
        |
        |Discrete transitions may be associated with events.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Hybrid_system"
    )
  ) { model =>
    import model._

    val size = sizeHint(default = 5)
    val swta = new SoftWTA(n = size)

    val tgt1 = swta(0)
    Trigger(tgt1 > 5.0, "rule1: inhib") {
      swta.inhib.update(10.0)
    }

    val tgt2 = swta(4)
    Trigger(tgt1 > 2.0 && tgt2 > 2.0, "rule2: inhib") {
      swta.inhib.update(8.0)
    }

    Trial(time = 40 s)
    Step(y = tgt1, on = 1 s, dur = 5 s, scale = 1.0).descOff = "rule1 should have triggered"

    Step(y = swta(2), on = 10 s, dur = 3 s, scale = 3.0).descOff = "no rules should have triggered"

    Step(y = tgt1, on = 15 s, dur = 20 s, scale = 3.0).descOff = "rule1 should have triggered again"

    Step(y = tgt2, on = 20 s, dur = 10 s, scale = 3.0).descOff = "rule2 should have triggered"
  }

  val preyPrey = ModelBuilder(
    "Predator Prey",
    Biology,
    tags = List("nonlinear", "first-order", "Lotka–Volterra"),
    library = this,
    hints = ModelHints(tau = 60.0, numberType = NumberType.Doubles, ode = OdeMethod.Ode45),
    topic = Topic(
      """The Lotka–Volterra equations, also known as the predator–prey equations, are a pair of first-order nonlinear differential equations, frequently used to describe the dynamics of biological systems in which two species interact, one as a predator and the other as prey. The populations change through time according to the pair of equations:
        |
        |    d x / d t = α x − β x y , d y / d t = δ x y − γ y
        |
        |where
        |
        |    x is the number of prey (for example, rabbits)
        |    y is the number of some predator (for example, foxes)
        |    d y / d t represents the instantaneous growth rates of the two populations
        |    t represents time
        |    α, β, γ, δ are positive real parameters describing the interaction of the two species.
        |
        |The Lotka–Volterra system of equations is an example of a Kolmogorov model, which is a more general framework that can model the dynamics of ecological systems with predator–prey interactions, competition, disease, and mutualism.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations"
    )
  ) { model =>
    import model._

    val predPrey = new PredPrey()
    import predPrey._

    Phase(x = pred, y = prey, scale = 40.0)
    Step(y = pred, on = 0, scale = 10.0)
    Step(y = prey, on = 150, dur = 20, scale = 10.0)
  }

  val duffing = ModelBuilder(
    "Forced Duffing",
    Mathematics,
    tags = List("chaos", "nonlinear", "Duffing"),
    library = this,
    topic = Topic(
      """The Duffing equation (or Duffing oscillator), named after Georg Duffing (1861–1944), is a non-linear second-order differential equation used to model certain damped and driven oscillators.
        |
        |The equation is given by x ¨ + δ x ˙ + α x + β x 3 = γ cos ( ω t )
        |
        |x   is the displacement at time t
        |x ˙ is the first derivative of x with respect to time, i.e. velocity
        |x ¨ is the second time-derivative of x , i.e. acceleration.
        |
        |The numbers δ , α , β , γ , and ω are given constants.
        |
        |The equation describes the motion of a damped oscillator with a more complex potential than in simple harmonic motion (which corresponds to the case β = δ = 0
        |
        |In physical terms, it models, for example, a spring pendulum whose spring's stiffness does not exactly obey Hooke's law.
        |
        |The Duffing equation is an example of a dynamical system that exhibits chaotic behavior.
        |
        |Moreover, the Duffing system presents in the frequency response the jump resonance phenomenon that is a sort of frequency hysteresis behaviour.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Duffing_equation"
    )
  ) { model =>
    import model._

    val duffing = new Duffing()
    import duffing._
    Phase(x, dx, window = 200, scale = 20)
    phasePlot.zoom = 2.0

    Cos(y = dx, on = 100, period = (2 * Math.PI * tau).toInt, cycles = 2000, scale = 16.5)
  }

  val rossler = ModelBuilder(
    "Rössler attractor",
    Mathematics,
    tags = List("chaos", "nonlinear", "Rössler"),
    library = this,
    topic = Topic(
      """The Rössler attractor is the attractor for the Rössler system, a system of three non-linear ordinary differential equations originally studied by Otto Rössler.
        |
        |These differential equations define a continuous-time dynamical system that exhibits chaotic dynamics associated with the fractal properties of the attractor.
        |
        |Some properties of the Rössler system can be deduced via linear methods such as eigenvectors, but the main features of the system require non-linear methods such as Poincaré maps and bifurcation diagrams.
        |
        |The original Rössler paper states the Rössler attractor was intended to behave similarly to the Lorenz attractor, but also be easier to analyze qualitatively.
        |
        |An orbit within the attractor follows an outward spiral close to the x,y plane around an unstable fixed point.
        |
        |Once the graph spirals out enough, a second fixed point influences the graph, causing a rise and twist in the z-dimension.
        |
        |In the time domain, it becomes apparent that although each variable is oscillating within a fixed range of values, the oscillations are chaotic.
        |
        |This attractor has some similarities to the Lorenz attractor, but is simpler and has only one manifold.
        |
        |Otto Rössler designed the Rössler attractor in 1976, but the originally theoretical equations were later found to be useful in modeling equilibrium in chemical reactions.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/R%C3%B6ssler_attractor"
    )
  ) { bld =>
    import bld._

    val rossler = new Rossler()
    import rossler._
    loc = Loc().left(250)
    Phase3D(x, y, z, scale = 15)

    Step(y = x, on = 0, scale = 2.0)
  }

  val lorentz = ModelBuilder(
    "Lorentz attractor",
    Mathematics,
    tags = List("chaos", "nonlinear", "butterfly", "Lorentz"),
    library = this,
    topic = Topic(
      """The Lorenz system is a system of ordinary differential equations first studied by Edward Lorenz.
        |
        |It is notable for having chaotic solutions for certain parameter values and initial conditions.
        |
        |In particular, the Lorenz attractor is a set of chaotic solutions of the Lorenz system.
        |
        |In popular media the 'butterfly effect' stems from the real-world implications of the Lorenz attractor, i.e. that in any physical system, in the absence of perfect knowledge of the initial conditions (even the minuscule disturbance of the air due to a butterfly flapping its wings), our ability to predict its future course will always fail.
        |
        |This underscores that physical systems can be completely deterministic and yet still be inherently unpredictable even in the absence of quantum effects. The shape of the Lorenz attractor itself, when plotted graphically, may also be seen to resemble a butterfly.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Lorenz_system"
    )
  ) { model =>
    import model._

    val lorentz = new Lorentz()
    import lorentz._
    loc = Loc().left(240)
    Phase3D(x, y, z)
    Step(y = y, on = 0)
  }

  val spikingModels = ModelBuilder(
    "Spiking Neurons",
    Neuroscience,
    tags = List("nonlinear", "spiking neural models", "bursting", "lif", "resonate-and-fire"),
    library = this
  ) { model =>
    import memnets.models.neuro.spiking._
    import model._
    val spike = 4.0

    val lif = Lif(name = "LIF", spike = spike)
    val oaf = new ResFire(spike = spike)
    import oaf._

    val user = Y(name = "User", decay = -1.0, act = Relu, showText = true)
    user.relumax = 10.0
    user.ui.viz = Viz.User

    user --> (lif, oaf)

    tracked ++= Seq(lif, na)

    system.onLayout = {
      user.ui.loc = Loc().down(140).left(100)
      lif.ui.loc = Loc().left(270)
      na.ui.loc = lif.ui.loc.down(160)
      k.ui.loc = na.ui.loc.down(80).right(100)
    }
    Phase(x = lif, y = oaf, window = 200, sampling = 1, scale = 20)

    val win = 80
    Tracer(lif, window = win)
    Tracer(oaf, window = win)

    Trial(time = 30 s, name = "step")
    Step(y = user, on = 1 s, dur = 5 s, scale = 3.0)
    Step(y = user, on = 10 s, dur = 5 s, scale = 5.0)
    Step(y = user, on = 20 s, dur = 5 s, scale = 9.0)
  }

  val resFire = ModelBuilder(
    "Resonate-and-Fire",
    Neuroscience,
    tags = List("nonlinear", "spiking neural models", "bursting"),
    library = this
  ) { model =>
    import model._

    val spikeThreshold = 6.0

    val raf = new ResFire(spike = spikeThreshold)
    import raf._
    system.onLayout = {
      na.ui.loc = Loc().left(160).down(20)
      k.ui.loc = na.ui.loc.down(120).left(120)
    }
    Phase(x = k, y = na, window = 200, sampling = 1, scale = 20)
    Tracer(na, window = 80)

    skin = Skin { x =>
      x.backImageOn = true
      x.backImage = SkinImage.FOUR
    }

    Trial(time = 30 s, name = "step")
    Step(y = na, on = 1 s, dur = 5 s, scale = 3.0)
    Step(y = na, on = 10 s, dur = 5 s, scale = 5.0)
    Step(y = na, on = 20 s, dur = 5 s, scale = 9.0)
  }

  val izhikevich = ModelBuilder(
    "Izhikevich Spiking",
    Neuroscience,
    tags = List("nonlinear", "spiking neural models", "Hodgkin-Huxley", "bursting"),
    library = this,
    topic = Topic(
      """Neuron behavior is often modeled as single-compartment, non-linear dynamical systems, where the neuron states represent physiological quantities such as membrane voltage, current flow, and the concentrations of various ions intra- and extracellularly.
        |
        |These models most generally take the singularly perturbed form
        |
        |    fast subsystem: x ˙ = f ( x , u )
        |    slow subsystem: u ˙ = μ g ( x , u )
        |
        |where f and g are both Hodgkin–Huxley style relations,
        |x ˙ is a vector representing the cell parameters relevant to the fast subsystem,
        |u ˙ is a vector representing the parameters of the slow modulation subsystem,
        |and μ ≪ 1 is the ratio of the time scales between the fast and slow subsystems.
        |
        |Models of neuron dynamics generally exhibit a number of stable and unstable attractors in phase space which represent resting states.
        |
        |When the system is sufficiently perturbed by input stimuli it may follow a complex return path back to the stable attractor representing an action potential.
        |
        |In bursting neurons, these dynamic spaces bifurcate between quiescent and bursting modes according to the dynamics of the slow system.
        |
        |These two bifurcations may take many forms and the choice of bifurcation both from quiescent to bursting and bursting to quiescent can affect the behavioral aspects of the burster.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Bursting"
    )
  ) { bld =>
    import bld._ // NOTE : using bld name so can use "b" below

    val izhik = new Izhikevich()
    import izhik._
    system.sparse.onSpike = t => spikeReset()
    system.onReset = { reset() }
    track(v)

    Phase(x = u, y = v, scale = 3.0)
    phasePlot.scale = 0.8

    skin = Skin { x =>
      x.backImageOn = true
//      x.backImage = SkinImage.FOUR
    }

    Trial(name = "step response", time = 40 s)
    Step(y = v, on = 1 s, dur = 5 s, scale = 20.0).desc = "slow spiking at 20v"
    Step(y = v, on = 10 s, dur = 5 s, scale = 10.0).desc = "no spikes at 10v"
    Step(y = v, on = 20 s, dur = 5 s, scale = 40.0).desc = "moderate spiking at 40v"
    Step(y = v, on = 30 s, dur = 5 s, scale = 80.0).desc = "fast spiking at 80v"
  }

  val pendulumWaves = ModelBuilder(
    "Pendulum Waves",
    Physics,
    tags = List("oscillation", "pendulum", "phase", "waves"),
    library = this,
    desc =
      """By monotonically increasing lengths of numerous uncoupled pendulums, they "dance" together to make visual traveling waves, standing waves, beating, and random motion.
        |""".stripMargin,
    topic = Topic(
      """A pendulum is a weight suspended from a pivot so that it can swing freely.
        |
        |When a pendulum is displaced sideways from its resting, equilibrium position, it is subject to a restoring force due to gravity that will accelerate it back toward the equilibrium position.  When released, the restoring force acting on the pendulum's mass causes it to oscillate about the equilibrium position, swinging back and forth.
        |
        |The time for one complete cycle, a left swing and a right swing, is called the period. The period depends on the length of the pendulum and also to a slight degree on the amplitude, the width of the pendulum's swing.
        |
        |From the first scientific investigations of the pendulum around 1602 by Galileo Galilei, the regular motion of pendulums was used for timekeeping, and was the world's most accurate timekeeping technology until the 1930s.
        |
        |The pendulum clock invented by Christian Huygens in 1658 became the world's standard timekeeper, used in homes and offices for 270 years, and achieved accuracy of about one second per year before it was superseded as a time standard by the quartz clock in the 1930s.
        |
        |Pendulums are also used in scientific instruments such as accelerometers and seismometers. Historically they were used as gravimeters to measure the acceleration of gravity in geophysical surveys, and even as a standard of length.
        |
        |The word "pendulum" is new Latin, from the Latin pendulus, meaning 'hanging'.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Pendulum"
    )
  ) { model =>
    import model._

    val baseFreq = 50.0
    val n = 36
    // technically, decay proportional to freq but using shared value
    val dampen = Param(name = "dampen", max = -0.1, init = -0.0001)
    val oscs = Array.tabulate(n) { j =>
      val i = j + 1
      val f = (baseFreq + i) / 25.0
      val osc = Osc(freq = f)
      osc.decay tie = dampen
      if (i == 1 || i == 5 | i == 9) track(osc)
      osc.x.ui.color = Colorf.hsb((320 + (i - 1) * 7) % 320, 0.8, 1.0, 1.0)
      osc
    }
    oscs.center(Loc().down(310), 34)

    system.onReset = {
      for (osc <- oscs) osc.init(phase = Math.PI / 2.0, scale = 10.0)
    }

    val ph = Phase(x = oscs(1), y = oscs(9), scale = 30.0)
    phasePlot.ui.loc = Loc().up(30)
    phasePlot.scale = 1.0
    phasePlot.zoom = 1.0

    onEvent = _ match {
      case SelectEvent(y) if y != null =>
        ph.update(y, oscs(1))
      case default =>
    }

    skin = Skin(_.backImage = SkinImage.THREE)
  }

  val oscPop = ModelBuilder(
    "Osc Population",
    Neuroscience,
    library = this,
    topic = CommonTopics.harmonicOscillator
  ) { model =>
    import model._

    autoReleaseRT = true
    val size = sizeHint(default = 128 * 128)

    val damping = Param("damping", max = -0.2, init = -0.001)
    val oscs = OscPop(size = size, freq = 1.0.toFreq(), dampingTie = damping)

    tracked += oscs.y.y(oscs.y.length - 1)

    oscs.y.ui.gridHints = GridHints(zoom3D = 2.0)
    skin = Skin { x =>
      x.layerVizType = LayerVizType.Sliding
      x.gridVizType = GridVizType.Mesh
    }

    Trial(name = "Phase Offset", time = 5 m).onReset = {
      var i = 0
      val ph: Double = 2.0 * Math.PI / oscs.size.toDouble
      while (i < oscs.size) {
        oscs.init(i = i, phase = ph * i, scale = 10.0)
        i += 1
      }
    }
    Trial(name = "Phase Offset Repeat", time = 5 m).onReset = {
      var i = 0
      val ph: Double = 4.0 * 2.0 * Math.PI / oscs.size.toDouble
      while (i < oscs.size) {
        oscs.init(i = i, phase = ph * i, scale = 10.0)
        i += 1
      }
    }
    Trial(name = "Random", time = 5 m).onReset = {
      var i = 0
      while (i < oscs.size) {
        oscs.init(i = i, phase = 2.0 * Math.PI * random(), scale = 10.0)
        i += 1
      }
    }
  }

  val oscPopHetero = ModelBuilder(
    "Osc Pop (Hetero)",
    Neuroscience,
    library = this,
    topic = CommonTopics.harmonicOscillator
  ) { model =>
    import model._

    autoReleaseRT = true
    val size = sizeHint(default = 128 * 128)
    val sizeD = size.toDouble
    val step = 8.0 / sizeD
    val oscs = HeteroOscPop(size = size, freq = i => 0.1 + i * step, damping = i => -0.001)

    tracked += oscs.y.y(oscs.y.length - 1)

    skin = Skin { x =>
      x.layerVizType = LayerVizType.Grid
    }

    Trial(name = "Phase Offset", time = 5 m).onReset = {
      var i = 0
      val ph: Double = 2.0 * Math.PI / sizeD
      while (i < oscs.size) {
        oscs.init(i = i, phase = ph * i, scale = 10.0)
        i += 1
      }
    }
    Trial(name = "Phase Offset Repeat", time = 5 m).onReset = {
      var i = 0
      val ph: Double = 4.0 * 2.0 * Math.PI / oscs.size.toDouble
      while (i < oscs.size) {
        oscs.init(i = i, phase = ph * i, scale = 10.0)
        i += 1
      }
    }
    Trial(name = "Random", time = 5 m).onReset = {
      var i = 0
      while (i < oscs.size) {
        oscs.init(i = i, phase = 2.0 * Math.PI * random(), scale = 10.0)
        i += 1
      }
    }
  }

  val fireflySync = ModelBuilder(
    "Firefly Sync",
    Biology,
    tags = List("phase synchronization", "leaky integrate+fire", "Strogatz"),
    library = this,
    topic = Topic(
      """Phase synchronization is the process by which two or more cyclic signals tend to oscillate with a repeating sequence of relative phase angles.
        |
        |Phase synchronisation is usually applied to two waveforms of the same frequency with identical phase angles with each cycle. However it can be applied if there is an integer relationship of frequency, such that the cyclic signals share a repeating sequence of phase angles over consecutive cycles. These integer relationships are called Arnold tongues which follow from bifurcation of the circle map.
        |
        |One example of phase synchronization of multiple oscillators can be seen in the behavior of Southeast Asian fireflies. At dusk, the flies begin to flash periodically with random phases and a gaussian distribution of native frequencies. As night falls, the flies, sensitive to one another's behavior, begin to synchronize their flashing. After some time all the fireflies within a given tree (or even larger area) will begin to flash simultaneously in a burst.
        |
        |Thinking of the fireflies as biological oscillators, we can define the phase to be 0° during the flash and +-180° exactly halfway until the next flash. Thus, when they begin to flash in unison, they synchronize in phase.
        |
        |One way to keep a local oscillator "phase synchronized" with a remote transmitter uses a phase-locked loop.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Phase_synchronization"
    )
  ) { model =>
    import model._

    val amp = 2.0
    val col = sizeHint(default = 70)
    val units = Array.tabulate(4 * col) { i =>
      val lif = Y(name = " ", decay = -1.0, tau = 20.0, scale = amp, threshold = -amp, act = Spike) // constant input
      lif.spike = amp - 0.01
      val off = (i / col) match {
        case 0 => 0
        case 1 => 100
        case 2 => 350
        case 3 => 450
      }
      lif.ui.loc = Loc(15 + (i % col) * 18, Loc().y + -120.0 + off)
      lif
    }
    system.sparse.ui.plot.range = YRange(0.0, DynamicSystem.SPIKE_AP)
    tracked ++= units.randomPicks(n = 7)

    val sync = Y(name = "sync", decay = -1.0, tau = 1.0, act = Relu, scale = 1.0)
    sync.ui.color = Colorf.INHIB

    // not using Spike_AP here b/c see edges better w/ small value, just inc weights
    val p = Y(name = " ", decay = -1.0, tau = 1.0, scale = amp)
    p.ui.color = Colorf.INHIB
    p.out = { d =>
      val act = sync.act
      if (act > 0.5 && system.now.lastSpikeCount > 0)
        act * amp
      else
        0.0
    }
    p --> units w = 0.005
    system.onLayout = {
      p.ui.loc = Loc().down()
      sync.ui.loc = p.ui.loc.right(350)
    }
    skin = Skin { x =>
      x.signalsOn = false
      x.edgeScale = 0.1
      x.zoom = 1.0
      x.bestQuality = false
    }

    val pdf = Gaussian(amp, amp)
    Trial(name = "sync", time = 2 m).onReset = {
      for (u <- units) {
        val s = pdf.sample()
        u.update(if (s > amp) s - amp else s)
      }
    }
    Step(y = sync, on = 400, dur = -1)
  }

  val coupled = ModelBuilder(
    "Coupled Oscillators",
    Physics,
    tags = List("phase sychronization", "coupled oscillators"),
    library = this,
    topic = coupledOscTopic
  ) { model =>
    import model._

    val coupled = new CoupledOsc()
    coupled.ui.loc = Loc().left(240).down(120)
    import coupled._

    track(oscA, oscB)
    Phase(oscA, oscB, scale = 30.0)
    system.onReset = {
      coupled.randomPhase(mag = 8.0)
    }
  }

  val coupledN = ModelBuilder(
    "N-Coupled Oscillators",
    Physics,
    tags = List("sychronization", "n-coupled oscillators", "phase locking"),
    library = this,
    topic = phaseSyncTopic
  ) { model =>
    import model._

    val size = sizeHint(default = 20)
    val oscs = Seq.tabulate(size) { i =>
      Osc(1.0.toFreq())
    }
    val coupling = Param("coupling", max = 1.0, init = 0.2)

    for (Seq(oscA, oscB) <- oscs.sliding(2))
      Coupling(oscA, oscB, coupling)

    Coupling(oscs.last, oscs.head, coupling)

    val oscTracked = Seq(oscs.head, oscs(oscs.length / 3), oscs(2 * oscs.length / 3))
    track(oscTracked: _*)

    val ph3d = Phase3D(oscTracked(0), oscTracked(1), oscTracked(2), scale = 50.0)
    phasePlot.ui.loc = Loc().up(60)

    system.onLayout = {
      oscs.center(Loc().down(300), spacing = 35)
    }
    onEvent = _ match {
      case SelectEvent(y) =>
        ph3d.update(oscTracked(0), oscTracked(1), if (y == null) oscTracked(2) else y)
      case default =>
    }
    Trial(name = "random phase").onReset = {
      for (osc <- oscs)
        osc.init(phase = 2.0 * Math.PI * random(), scale = 20.0)
    }
  }

  val speech = ModelBuilder(
    "Speech Production",
    Cognitive,
    tags = List("speech production", "inhibition of return", "IOR", "parallel processing", "sWTA"),
    library = this,
    hints = ModelHints(tau = 10),
    desc =
      """A soft-winner-take-all (sWTA) model combined with inhibition of return (IOR) generates a flow between units.
        |
        |In this model, each word has top-down links to letter units.  Each letter has it's own IOR unit.
        |
        |The strongest weight peaks first, then the IOR pushes it down, allowing the next strong weight to rise.
        |""".stripMargin,
    topic = Topic(
      """Speech production is the process by which thoughts are translated into speech. This includes the selection of words, the organization of relevant grammatical forms, and then the articulation of the resulting sounds by the motor system using the vocal apparatus.
        |
        |Speech production can be spontaneous such as when a person creates the words of a conversation, reactive such as when they name a picture or read aloud a written word, or imitative, such as in speech repetition.
        |
        |Speech production is not the same as language production since language can also be produced manually by signs.
        |
        |In ordinary fluent conversation people pronounce roughly four syllables, ten or twelve phonemes and two to three words out of their vocabulary (that can contain 10 to 100 thousand words) each second.
        |
        |Errors in speech production are relatively rare occurring at a rate of about once in every 900 words in spontaneous speech.
        |
        |Words that are commonly spoken or learned early in life or easily imagined are quicker to say than ones that are rarely said, learnt later in life, or are abstract.
        |""".stripMargin
    )
  ) { model =>
    import model._

    val labels = "aeioudrstnmcb".toUpperCase
    val swta = new SoftWTA(n = labels.length)
    swta.inhib.tau = Math.max(1.0, system.tau / 2.0)
    swta.layoutHelper(ctr = Loc().down(50), vspacing = 250)
    val clear = Y("Reset", decay = -1.0, act = Relu, tau = 20)
    clear.relumax = 8.0
    clear.ui.loc = swta.inhib.ui.loc.right(200)

    val iors = ArrayBuffer[Y]()
    val name2y = mutable.HashMap[Char, Y]()
    for ((e, ch) <- swta.zip(labels.toCharArray)) {
      e.name = ch.toString
      e.threshold = 0.5
      name2y(ch) = e
      val ior = Y(name = "-" + ch, act = Relu, tau = 2.5 * system.tau)
      iors += ior
      ior.ui.color = Colorf.GRAY
      e --> ior w = 0.1
      ior --> e w = -0.5
      clear --> ior w = -10.0
      ior.ui.loc = e.ui.loc.up(120).right(20)
    }
    val words = Seq("cub", "dim", "rent", "dream", "store")
    val wordUnits = words.map { w =>
      val s = w.toUpperCase()
      val unit = Y(name = s, decay = -1.0, act = Relu, scale = 2.0)
      unit.relumax = 3.0
      val children = s.toCharArray.map(name2y(_))
      for ((c, i) <- children.zipWithIndex)
        unit --> c w = 1.0 - i * 0.15 // should check for max length

      if (w == "cub") tracked ++= children
      unit
    }
    wordUnits.center(Loc().up(100), spacing = 100)

    system.onLayout = NULL_LAYOUT
    for (word <- wordUnits) {
      Trial(name = word.name, time = 30 s)
      Step(y = word, dur = 1000, scale = 2.5)
    }
  }

  val pdController = ModelBuilder(
    "PD Controller",
    Engineering,
    tags = List("control systems", "feedback", "PD controller", "proportional-derivative"),
    library = this,
    hints = ModelHints(ode = OdeMethod.Euler)
  ) { model =>
    import model._

    val pd = new PD()
    import pd._
    tracked ++= Seq(tgt, out)
    Trial(name = "square")
    Square(y = tgt, on = 100, period = 360, scale = 8.0)
  }

  val fsm = ModelBuilder(
    "Finite State Machine",
    Neuroscience,
    tags = List("finite state machine", "sWTA", "Rutishauser", "Douglas"),
    library = this,
    desc =
      """This model uses a soft-winner-take-all (sWTA) network with units for each state transition to create a finite-state machine (FSM).
        |
        |The FSM models a TV with 3 states (on, off, wide).
        |
        |Pushing "wide" on a remote does nothing until the TV is "on".
        |
        |Once a TV is "on", you can switch to "wide" mode.  Power "off" can happen in either "on" or "wide" mode.
        |""".stripMargin,
    topic = Topic(
      """A finite-state machine (FSM) or finite-state automaton (FSA, plural: automata), finite automaton, or simply a state machine, is a mathematical model of computation.
        |
        |It is an abstract machine that can be in exactly one of a finite number of states at any given time.
        |
        |The FSM can change from one state to another in response to some external inputs; the change from one state to another is called a transition.
        |
        |An FSM is defined by a list of its states, its initial state, and the conditions for each transition.
        |
        |Finite state machines are of two types – deterministic finite state machines and non-deterministic finite state machines.
        |
        |A deterministic finite-state machine can be constructed equivalent to any non-deterministic one.
        |
        |The behavior of state machines can be observed in many devices in modern society that perform a predetermined sequence of actions depending on a sequence of events with which they are presented.
        |
        |Simple examples are vending machines, which dispense products when the proper combination of coins is deposited, elevators, whose sequence of stops is determined by the floors requested by riders, traffic lights, which change sequence when cars are waiting, and combination locks, which require the input of a sequence of numbers in the proper order.
        |
        |The finite state machine has less computational power than some other models of computation such as the Turing machine.
        |
        |The computational power distinction means there are computational tasks that a Turing machine can do but a FSM cannot.
        |
        |This is because a FSM's memory is limited by the number of states it has. FSMs are studied in the more general field of automata theory.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Finite-state_machine"
    )
  ) { model =>
    import model._

    autoReleaseRT = true
    val fsm = FSM("off", "on", "wide")
    val off = fsm(0)
    val on = fsm(1)
    val wide = fsm(2)
    off --> (on, "power")
    on --> (off, "power")
    wide --> (off, "power")
    on --> (wide, "picture")
    wide --> (on, "picture")
    fsm.build()

    on.y.ui.color = Colorf.LIMEGREEN
    off.y.ui.color = Colorf.CRIMSON
    wide.y.ui.color = Colorf.YELLOW.huef(-10).satf(0.8)

    val power = fsm.input("power")
    val picture = fsm.input("picture")
    power.ui.color = on.y.ui.color.get.huef(20)
    picture.ui.color = wide.y.ui.color.get.huef(20)

    tracked ++= List(on.y, off.y) ++ fsm.inputs

    skin = Skin { x =>
      x.signalsOn = false
      x.zoom = 1.3
    }

    Trial(time = 1500, name = "transitions")
    Step(y = picture, on = 100, dur = 40, scale = 10.0).desc = "picture does nothing if off"
    Step(y = power, on = 300, dur = 40, scale = 10.0).desc = "turn power on"
    Step(y = picture, on = 500, dur = 40, scale = 10.0).desc = "picture mode on"
    Step(y = picture, on = 700, dur = 40, scale = 10.0).desc = "picture mode off"
    Step(y = picture, on = 900, dur = 40, scale = 10.0).desc = "picture mode on"
    Step(y = power, on = 1100, dur = 40, scale = 10.0).desc = "turn power off in picture mode"
  }

  val logo = ModelBuilder(
    "MemNets Logo",
    Neuroscience,
    tags = List("sWTA", "chain", "noise"),
    library = this,
    hints = ModelHints(tau = 14),
    desc =
      """This model uses a soft-winner-take-all (sWTA) with a strong feed-forward lateral chain between the each letter in the logo.
        |
        |To make the logo more interesting, we've added some background noise.
      """.stripMargin,
    topic = swtaTopic
  ) { model =>
    import model._

    val memnets = "MemNets"
    val swta = new SoftWTA(memnets.length)
    var start = 260.0 // purple
    var j = 0
    for ((e, i) <- swta.zipWithIndex) {
      val letter = memnets.charAt(i).toString
      e.name = letter
      e.ui.scale = 1.5
      if (letter == "N") {
        start = 200.0 // blue
        j = 0
      }
      e.ui.color = Colorf.hsb(start + j * 12, 0.8, 1.0, 1.0)
      j += 1
    }
    swta.e2eT.value = 0.4
    swta.inhib.tau = Math.max(1.0, system.tau / 2.0)
    swta.last.ui.color = Colorf.DEEPPINK
    chain(swta, 0.9, KernelType.Wrap)
    system.noise.value = 0.15

    swta.inhib.threshold = 0.0
    val speed = Y(" ", tau = 150.0, act = Relu)
    speed.relumax = 1.0
    speed.ui.color = Colorf.ACC_COLOR
    swta.last --> speed
    speed --> swta.inhib w = -1.0

    val combo = Y(" ", tau = 1.0, threshold = 1.0, act = Relu, scale = 1.0)
    combo.ui.color = Colorf.INHIB
    combo.noiseScale = 0.0
    speed --> combo w = 0.95 // 0.9 at max
    swta.last --> combo w = 0.02 // when 5.0, 0.05

    val off = Y(" ", decay = -0.01, tau = 2.0, act = Relu, scale = 30.0)
    off.ui.color = Colorf.LOSE_COLOR
    off.noiseScale = 0.0
    off --> swta.inhib w = 0.1
    combo --> off w = 20.0

    tracked ++= swta
    tracked += speed
    system.onLayout = {
      swta.layout()
      //   speed.hide()
      List(off, combo, speed).horizontal(loc = swta.inhib.ui.loc.right(140), spacing = 65)
    }

    skin = Skin { _.backImageOn = true }

    Trial(time = 2 m, name = "logo")
  }

  // PageRank section
  val pageRank = ModelBuilder(
    "PageRank",
    ComputerSci,
    tags = Seq("machine learning"),
    library = this,
    topic = pageRankTopic
  ) { cxt =>
    import cxt._

    val pageRank = new PageRank(showJump = true)
    import pageRank._
    val a = page("A")
    val b = page("B")
    val c = page("C")
    val d = page("D")

    a --> b
    a --> c
    b --> c
    c --> a
    d --> c

    updateWeights()

    tracked ++= Seq(a, d)
    system.onLayout = {
      val sp = 140
      val ctr2 = Loc().down(60)
      a.ui.loc = ctr2.left(sp).up(sp)
      b.ui.loc = ctr2.right(sp).up(sp)
      c.ui.loc = ctr2
      j.ui.loc = ctr2.left(sp).down(sp)
      d.ui.loc = ctr2.right(sp).down(sp)
    }
  }

  val pageRankHier = ModelBuilder(
    "PageRank Hierarchy",
    ComputerSci,
    tags = Seq("machine learning"),
    library = this,
    topic = pageRankTopic
  ) { cxt =>
    import cxt._

    val pageRank = new PageRank()
    import pageRank._
    val home = page("Home")
    val about = page("About")
    val product = page("Prod")
    val links = page("Links")

    val externSites = IndexedSeq("Ext A", "Ext B", "Ext C", "Ext D").map(page(_))

    val internal = List(about, product, links)
    home --> internal
    internal --> home
    links --> externSites
    externSites --> home

    j.ui.hide()
    updateWeights()

    tracked ++= Seq(home, links, externSites(0))
    system.onLayout = {
      val sp = 160
      home.ui.loc = Loc().down(80)
      j.ui.loc = home.ui.loc.right(100)

      internal.center(home.ui.loc.down(sp), 320)
      externSites.center(home.ui.loc.up(sp), spacing = 120)
    }
  }

  // 2D secttion
  val wave2D = ModelBuilder(
    "Wave Equation 2D",
    Physics,
    tags = Seq("2D", "partial DE", "fluid dynamics"),
    library = this,
    topic = Topic(
      """The wave equation is an important second-order linear partial differential equation for the description of waves—as they occur in classical physics—such as mechanical waves (e.g. water waves, sound waves and seismic waves) or light waves.
        |
        |It arises in fields like acoustics, electromagnetics, and fluid dynamics.
        |
        |Historically, the problem of a vibrating string such as that of a musical instrument was studied by Jean le Rond d'Alembert, Leonhard Euler, Daniel Bernoulli, and Joseph-Louis Lagrange.
        |
        |In 1746, d’Alembert discovered the one-dimensional wave equation, and within ten years Euler discovered the three-dimensional wave equation.
        |""".stripMargin,
      "https://en.wikipedia.org/wiki/Wave_equation"
    ),
    hints = ModelHints(numberType = NumberType.Floats, ode = OdeMethod.LeapFrog)
  ) { model =>
    import model._

    autoReleaseRT = true
    val size = sizeHint(default = 256)
    val tau = 2
    val grid = YGrid(rows = size, cols = size, tau = tau, initKernel = YGrid.FullyConnected) // CFL < 0.5 ok
    grid.ui.loc = Loc().up(50)
    val cMid, rMid = grid.rows / 2
    val len = 41
    val units = for (c <- 0 until len) yield {
      val y = grid(rMid, cMid + c - len / 2)
      y.name = " "
      y.ui.viz = Viz.Default
      if (c == 0) tracked += y
      if (c == len - 1) tracked += y
      y
    }
    units.center(Loc().down(300), spacing = 30)
    system.sparse.ui.gradient = GradientHints(hue = 80, maxLength = 300)

    skin = Skin { x =>
      x.colorMapType = ColorMapType.OpacityMap
      x.backImageOn = false
      x.backColor = Colorf.BLACK
    }

    Trial(name = "center input")
    var on = 100
    val d = 2 * tau.toInt
    val pt = size / 2
    val amp = 100.0
    Step(y = grid(pt, pt), on = on, dur = d, scale = amp)
    Step(grid(pt + 1, pt), on = on, dur = d, scale = amp / 2)
    Step(grid(pt - 1, pt), on = on, dur = d, scale = amp / 2)
    Step(grid(pt, pt + 1), on = on, dur = d, scale = amp / 2)
    Step(grid(pt, pt - 1), on = on, dur = d, scale = amp / 2)
    on += d
    Step(grid(pt, pt), on = on, dur = d, scale = -amp)
    Step(grid(pt + 1, pt), on = on, dur = d, scale = -amp / 2)
    Step(grid(pt - 1, pt), on = on, dur = d, scale = -amp / 2)
    Step(grid(pt, pt + 1), on = on, dur = d, scale = -amp / 2)
    Step(grid(pt, pt - 1), on = on, dur = d, scale = -amp / 2)
  }

  val heat = ModelBuilder(
    "Heat Equation",
    Physics,
    tags = Seq("2D", "partial DE", "thermodynamics", "diffusion equation"),
    library = this,
    topic = heatTopic,
    hints = NumberType.Floats
  ) { model =>
    import model._

    val size = sizeHint(default = 256)
    val tau = 8
    val grid = YGrid(size, size, tau, initKernel = YGrid.FullyConnected)
    grid.ui.loc = Loc().up(50)
    val rMid = grid.rows / 2
    val cMid = grid.cols / 2
    val len = 41
    for (c <- 0 until len) {
      val y = grid(rMid, cMid + c - len / 2)
      y.name = " "
      y.ui.viz = Viz.Default
      y.ui.loc = Loc(40 + c * 30, 680)
      if (c == 0) tracked += y
      if (c == len - 1) tracked += y
    }
    system.sparse.ui.gradient = GradientHints(maxLength = len)

    skin = Skin { _.chartOn = false }

    Trial(name = "2 sources")
    val dur = 60 s
    val off = Math.min(size / 16, 8)
    Step(grid(rMid, cMid - off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid + 1, cMid - off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid, cMid + off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid + 1, cMid + off), on = 200, dur = dur, scale = 10)
  }

  val heatGrid = ModelBuilder(
    "Heat Equation (LinkGrid)",
    Physics,
    tags = Seq("2D", "partial DE", "thermodynamics"),
    library = this,
    topic = heatTopic,
    hints = NumberType.Floats
  ) { model =>
    import model._

    val size = sizeHint(default = 256)
    val tau = 8
    val grid = LinkGrid(size, size, tau, initKernel = LinkGrid.FullyConnected)
    grid.ui.loc = Loc().up(50)
    val cMid = grid.cols / 2
    val rMid = grid.rows / 2

    Trial(name = "input")
    val dur = 60 s
    val off = Math.min(size / 16, 8)
    Step(grid(rMid, cMid - off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid + 1, cMid - off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid, cMid + off), on = 200, dur = dur, scale = 10)
    Step(grid(rMid + 1, cMid + off), on = 200, dur = dur, scale = 10)
  }

  val lifPop = ModelBuilder(
    "LIF Population",
    Biology,
    tags = List("phase synchronization", "leaky integrate+fire", "Strogatz"),
    library = this,
    desc =
      """EPILEPSY WARNING: this visualization produces rapid flashing lights that could be an issue for those with epilepsy.
        |""".stripMargin,
    topic = CommonTopics.phaseSyncTopic
  ) { model =>
    import model._

    val amp = 5.0
    val fire = amp - 0.03

    val size = sizeHint(default = 128)
    val master = Y(name = " ", decay = -1.0, act = Spike, threshold = -amp, scale = amp)
    master.spike = fire
    val relay = Y(name = "out", decay = -1.0, tau = 1.0, act = Relu, scale = amp)
    master --> relay

    val gridSizeD = (size * size).toDouble
    val grid = new YGrid(rows = size, cols = size, decay = -1.0) {
      override def create(r: Int, c: Int): Y = {
        val y = Y(decay = decay, tau = tau, act = Spike, threshold = -amp)
        y.ui.hide()
        y.spike = fire
        relay --> y w = 0.2
        y --> master w = 0.25 / gridSizeD
        y
      }
    }
    val inhib = Y(name = "inhib", threshold = -1.0, decay = -1.0, tau = 1.0, act = Relu, scale = 1.0)
    inhib --> relay w = -1000.0

    val spiking = Y(name = "sync%", decay = -1.0, tau = 1.0, scale = 1.0)
    spiking.f(desc = "pop") { t =>
      system.now.lastSpikeCount.toDouble / gridSizeD
    }

    tracked += spiking

    spiking.ui.color = Colorf.DEEPPINK
    master.ui.color = Colorf.DEEPPINK.huef(15.0)
    relay.ui.color = Colorf.DEEPPINK.huef(30.0)
    inhib.ui.color = Colorf.INHIB

    Tracer(spiking, window = 120, sampling = 1)

    system.onLayout = {
      grid.ui.loc = Loc()
      spiking.ui.loc = Loc().right(400).down(285)
      master.ui.loc = spiking.ui.loc.up(200)
      relay.ui.loc = master.ui.loc.up(200)
      inhib.ui.loc = master.ui.loc.right(120)
    }
    skin = Skin { x =>
      x.signalsOn = false
      x.colorMapType = ColorMapType.OpacityMap
      x.backColor = Colorf.BLACK
      x.chartSampling = 1
      x.zoomAuto = false
    }
    val pdf = Gaussian(amp, 2.0 * amp)
    Trial(1.5 m).onReset = {
      for { y <- grid } {
        val act = pdf.sample()
        y.update(if (act >= fire) act - amp else amp)
      }
    }
    Step(y = inhib, on = 8 s, dur = -1, scale = -2.0).desc = "Releasing inhibition"
  }
}
