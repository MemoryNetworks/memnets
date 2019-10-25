package memnets.models

import breeze.linalg._
import memnets.core._
import memnets.model._
import memnets.ui._
import memnets.utils._

/** examples of the API / Domain Specific Language (DSL)   */
object DslExamples extends Library {
  val dslTags = List("API", "DSL", "example")

  val dslSparse = ModelBuilder(
    "DSL Sparse",
    tags = dslTags ++ List("SHM", "Oscillator"),
    library = this,
    desc = """This example of the Sparse API shows how to make an oscillator
             |
             |you can copy the code below and paste into the editor:
             |
             |val freq = 0.5.toFreq()
             |val dampen = 0.01
             |
             |val y = Y(name = "y")
             |val x = Y(name = "x", decay = -dampen) // y'' = -dampen * y'
             |
             |x --> y                   // y' = x
             |y --> x w = -freq * freq  // y'' = - f^2 * y
             |
             |// can specify what the time chart shows on top or let the system guess
             |model.track(y)
             |
             |model.onTick = { te =>
             |  if (te.modSec(2))
             |    logger.debug(f"y = ${y.act}%.02f at ${te.secs} sec")
             |}
             |
             |Trial(name = "IC by Step", time = 1 m)
             |// on = 0 sets initial condition (system forces duration = 0)
             |Step(y = y, on = 0, scale = 10.0)
             |
             |Trial(name = "IC by onReset", time = 1 m)
             |  .onReset = { y.update(10.0) }
             |
             |Trial(name = "sin input", time = 1 m)
             |Sin(y = y, on = 1 s, period = 0.5.toPeriod, phase = Math.PI, scale = 0.5)
             |""".stripMargin
  ) { b =>
    import b._ // brings in DSL implicits

    val freq = 0.5.toFreq()
    val dampen = 0.01

    val y = Y(name = "y")
    val x = Y(name = "x", decay = -dampen) // y'' = -dampen * y'

    x --> y // y' = x
    y --> x w = -freq * freq // y'' = - f^2 * y

    // can specify what the time chart shows on top or let the system guess
    track(y)

    system.onTick = { te =>
      if (te.modSec(2))
        logr.debug(f"y = ${y.act}%.02f at ${te.secs} sec")
    }

    Trial(name = "IC by Step", time = 1 m)
    // on = 0 sets initial condition (system forces duration = 0)
    Step(y = y, on = 0, scale = 10.0)

    Trial(name = "IC by onReset", time = 1 m).onReset = { y.update(10.0) }

    Trial(name = "sin input", time = 1 m)
    Sin(y = y, on = 1 s, period = 0.5.toPeriod, phase = Math.PI, scale = 0.5)
  }

  val dslOsc = ModelBuilder(
    "DSL Osc",
    tags = dslTags,
    library = this,
    desc = """This example of the Osc element shows how to make an oscillator with parameters
             |
             |you can copy the code below and paste into the editor:
             |
             |val frequency = Param("frequency", max = 2.0, init = 0.5, precision = 1e-4)
             |frequency.desc = s"Units in cycles/sec. A higher freq gives more cycles. Typically shown by ${GREEK.OMEGA}"
             |frequency.func = d => {
             |  val f = d.toFreq()
             |  - f * f
             |}
             |val damping = Param("damping", max = -0.2, init = -0.01, precision = 1e-4)
             |damping.desc = "The viscous damping coefficient. Typically shown by c.  Using negative c values"
             |damping.func = _ * model.tau/DynamicSystem.TAU_DEFAULT
             |
             |val osc = Osc(frequency, damping)
             |
             |// can specify what the time chart shows on top or let the system guess
             |model.track(osc)
             |
             |
             |model.onTick = { te =>
             |  if (te.modSec(2))
             |    logger.debug(f"y = ${osc.y.act}%.02f at ${te.secs} sec")
             |}
             |
             |// useful for debugging or testing results at the end
             |validator = { te =>
             |  if (te.modSec(8))
             |    logger.debug(f"validator: y = ${osc.y.act}%.02f at ${te.secs} sec")
             |}
             |
             |Trial(name = "IC by Step", time = 1 m)
             |Step(y = osc, on = 0, scale = 10.0)
             |
             |Trial(name = "IC by Osc.ics", time = 1 m)
             |osc.ics(phase = Math.PI, scale = 10.0)
             |
             |Trial(name = "IC by Osc.init", time = 1 m).onReset = {
             |  osc.init(phase = Math.PI/4.0, scale = 10.0)
             |}
             |""".stripMargin
  ) { b =>
    import b._

    val frequency = Param("frequency", max = 2.0, init = 0.5, precision = 1e-4)
    frequency.desc = s"Units in cycles/sec. A higher freq gives more cycles. Typically shown by ${GREEK.OMEGA}"
    frequency.func = d => {
      val f = d.toFreq()
      -f * f
    }
    val damping = Param("damping", max = -0.2, init = -0.01, precision = 1e-4)
    damping.desc = "The viscous damping coefficient. Typically shown by c.  Using negative c values"
    damping.func = _ * system.tau / DynamicSystem.TAU_DEFAULT

    val osc = Osc(frequency, damping)

    // can specify what the time chart shows on top or let the system guess
    track(osc)

    system.onTick = { te =>
      if (te.modSec(2))
        logr.debug(f"y = ${osc.y.act}%.02f at ${te.secs} sec")
    }

    // useful for debugging or testing results at the end
    validator = { te =>
      if (te.modSec(8))
        logr.debug(f"validator: y = ${osc.y.act}%.02f at ${te.secs} sec")
    }

    Trial(name = "IC by Step", time = 1 m)
    Step(y = osc, on = 0, scale = 10.0)

    Trial(name = "IC by Osc.ics", time = 1 m)
    osc.ics(phase = Math.PI, scale = 10.0)

    Trial(name = "IC by Osc.init", time = 1 m).onReset = {
      osc.init(phase = Math.PI / 4.0, scale = 10.0)
    }
  }

  val dslLayer = ModelBuilder(
    "DSL Layer",
    tags = dslTags,
    library = this,
    desc = """This example of the Layer API shows how to make lots of oscillators with parameters.
             |
             |It also uses a custom skin to the osc.y layer as a mesh.
             |
             |you can copy the code below and paste into the editor:
             |
             |val size = sizeHint(default = 4096)
             |
             |val freq = Param("frequency", max = 1.0, init = 0.5)
             |freq.desc = s"Units in cycles/sec. A higher freq gives more cycles. Typically shown by ${GREEK.OMEGA}"
             |freq.func = d => {
             |  val f = d.toFreq()
             |  - f * f
             |}
             |
             |val damping = Param("dampen", max = -1.0, init = -0.01)
             |damping.desc = "The viscous damping coefficient. Typically shown by c.  Using negative c values"
             |damping.func = _ * model.tau/DynamicSystem.TAU_DEFAULT
             |
             |val y = Layer(n = size, name = "y")
             |val x = Layer(n = size, name = "x")
             |
             |
             |x --> y              // y' = x
             |y --> x tie = freq   // y'' = - f^2 * y
             |x --> x tie = damping // y'' = - dampen * y'
             |
             |x.ui.skip()
             |
             |// sparse plot
             |y.ui.gradient = Colorf.DEEPPINK
             |y.ui.plot.height = 400
             |y.ui.plot.center() // call after change dimensions
             |
             |// customize grid viz in mesh/image by giving hints
             |y.ui.gridHints = GridHints(
             |  showGlass = true,
             |  showBorder = false,
             |  zoom3D = 3.0,
             |  width3D = 800,
             |  height3D = 400,
             |)
             |
             |// using custom skin, but could get similar results by using Skin editor
             |skin = Skin { x =>
             |  x.layerVizType = LayerVizType.Grid
             |  x.gridVizType = GridVizType.Mesh
             |}
             |
             |Trial(time = 10 m, name = "Phase Shift").onReset = {
             |  val scale = 10.0
             |  val phaseShift = 2.0 * Math.PI / y.length
             |  var i = 0
             |  while (i < y.length) {
             |    // sin(wt + ph)
             |    y(i) = scale * Math.sin(i * phaseShift)
             |    // y' = x ==> x = y'
             |    // w * cos(wt + ph)
             |    x(i) = scale * freq.value * Math.cos(i * phaseShift)
             |    i += 1
             |  }
             |}
             |
             |Trial(time = 10 m, name = "Random Phase").onReset = {
             |  val scale = 10.0
             |  var i = 0
             |  while (i < y.length) {
             |    val phaseShift = 2.0*Math.PI*Math.random()
             |    // sin(wt + ph)
             |    y(i) = scale * Math.sin(i * phaseShift)
             |    // y' = x ==> x = y'
             |    // w * cos(wt + ph)
             |    x(i) = scale * freq.value * Math.cos(i * phaseShift)
             |    i += 1
             |  }
             |}
             |""".stripMargin
  ) { b =>
    import b._

    val size = sizeHint(default = 4096)

    val freq = Param("frequency", max = 1.0, init = 0.5)
    freq.desc = s"Units in cycles/sec. A higher freq gives more cycles. Typically shown by ${GREEK.OMEGA}"
    freq.func = d => {
      val f = d.toFreq()
      -f * f
    }

    val damping = Param("dampen", max = -1.0, init = -0.01)
    damping.desc = "The viscous damping coefficient. Typically shown by c.  Using negative c values"
    damping.func = _ * system.tau / DynamicSystem.TAU_DEFAULT

    val y = Layer(n = size, name = "y")
    val x = Layer(n = size, name = "x")

    x --> y // y' = x
    y --> x tie = freq // y'' = - f^2 * y
    x --> x tie = damping // y'' = - dampen * y'

    x.ui.skip()

    // sparse plot
    y.ui.gradient = Colorf.DEEPPINK
    y.ui.plot.height = 400
    y.ui.plot.center() // call after change dimensions

    // customize grid viz in mesh/image by giving hints
    y.ui.gridHints = GridHints(
      showGlass = true,
      showBorder = false,
      zoom3D = 3.0,
      width3D = 800,
      height3D = 400,
    )

    // using custom skin, but could get similar results by using Skin editor
    skin = Skin { x =>
      x.layerVizType = LayerVizType.Grid
      x.gridVizType = GridVizType.Mesh
    }

    Trial(time = 10 m, name = "Phase Shift").onReset = {
      val scale = 10.0
      val phaseShift = 2.0 * Math.PI / y.length
      var i = 0
      while (i < y.length) {
        // sin(wt + ph)
        y(i) = scale * Math.sin(i * phaseShift)
        // y' = x ==> x = y'
        // w * cos(wt + ph)
        x(i) = scale * freq.value * Math.cos(i * phaseShift)
        i += 1
      }
    }
    Trial(time = 10 m, name = "Random Phase").onReset = {
      val scale = 10.0
      var i = 0
      while (i < y.length) {
        val phaseShift = 2.0 * Math.PI * Math.random()
        // sin(wt + ph)
        y(i) = scale * Math.sin(i * phaseShift)
        // y' = x ==> x = y'
        // w * cos(wt + ph)
        x(i) = scale * freq.value * Math.cos(i * phaseShift)
        i += 1
      }
    }
  }

  val dslMatrix = ModelBuilder(
    "DSL Matrix",
    tags = dslTags,
    library = this,
    desc = """This example of the DenseMatrix API show how to create a model directly from a matrix
             |
             |you can copy the code below and paste into the editor:
             |
             |val f = 0.5.toFreq()
             |val damping = -0.01
             |
             |val matrix = DenseMatrix(
             |  (0.0,  1.0),
             |  (-f*f, damping)
             |)
             |// if empty model, these should be the same as sparse.variables
             |val variables = model.sparse.fromMatrix(matrix)
             |
             |skin = Skin { x =>
             |  x.backImageOn = true
             |  x.backImage = SkinImage.FOUR
             |  x.zoom = 2.0
             |}
             |
             |// if only one Trial, can just add inputs
             |// IC if on = 0
             |Step(y = variables(0), on = 0, scale = 10.0)
             |""".stripMargin
  ) { b =>
    import b._

    val f = 0.5.toFreq()
    val damping = -0.01

    val matrix = DenseMatrix(
      (0.0, 1.0),
      (-f * f, damping)
    )
    // if empty model, these should be the same as sparse.variables
    val variables = system.sparse.fromMatrix(matrix)

    skin = Skin { x =>
      x.backImageOn = true
      x.backImage = SkinImage.FOUR
      x.zoom = 2.0
    }

    // if only one Trial, can just add inputs
    // IC if on = 0
    Step(y = variables(0), on = 0, scale = 10.0)
  }

  val dslMatrixCsvFile = ModelBuilder(
    "DSL Matrix from .csv",
    tags = dslTags,
    library = this,
    desc = """This example shows how to import a matrix/model from a .csv file
             |
             |you can copy the code below and paste into the editor:
             |
             |// if you have a .csv file, you can create matrix directly
             |val csvFile = fileHint.getOrElse {
             |  // if no file in config, read from jar so need to copy to tmp file
             |  "tutorial-matrix.csv".streamToTempFile().getAbsolutePath
             |}
             |val matrix = csvread(file = csvFile)
             |
             |// if empty model, these should be the same as sparse.variables
             |val variables = model.sparse.fromMatrix(matrix)
             |
             |skin = Skin { x =>
             |  x.backImageOn = true
             |  x.backImage = SkinImage.FOUR
             |  x.zoom = 2.0
             |}
             |
             |// if only one Trial, can just add inputs
             |// IC if on = 0
             |Step(y = variables(0), on = 0, scale = 10.0)
             |""".stripMargin
  ) { b =>
    import b._

    // if no file in config, read from jar so need to copy to tmp file
    lazy val default = "tutorial-matrix.csv".streamToTempFile().getAbsolutePath

    // if you have a .csv file, you can create matrix directly
    val csvFile = fileHint(default)
    val matrix = csvread(file = csvFile)

    // if empty model, these should be the same as sparse.variables
    val variables = system.sparse.fromMatrix(matrix)

    skin = Skin { x =>
      x.backImageOn = true
      x.backImage = SkinImage.FOUR
      x.zoom = 2.0
    }

    // if only one Trial, can just add inputs
    // IC if on = 0
    Step(y = variables(0), on = 0, scale = 10.0)
  }

  val dslLambdaLayer = ModelBuilder(
    "DSL Lambda Layer",
    tags = dslTags,
    library = this,
    desc = """This example shows how to import a matrix/model from a .csv file
             |
             |you can copy the code below and paste into the editor:
             |
             |val n = 64
             |// layers must be same length
             |val input = Layer(n = n, decay = -1.0, tau = 5.0, name = "x")
             |val lambda = LambdaLayer(n = n, f = Math.pow(_, 2.0), name = "x^2")
             |
             |input --> lambda
             |
             |// set custom color
             |input.ui.gradient = Colorf.hsb(40.0)
             |lambda.ui.gradient = Colorf.hsb(80.0)
             |
             |// layers each have own plot by default, so replace with custom plot
             |val plot = Plot(input, lambda)
             |plot.range = YRange(-5.0, 5.0)
             |plot.showZeroLine = true
             |plot.height = 400
             |plot.center()
             |
             |val i = n/4
             |model.tracked ++= Seq(input.y(i), lambda.y(i))
             |
             |Cos(y = input.y(i), on = 1 s, period = 3 s, scale = 2.0)
             |""".stripMargin
  ) { b =>
    import b._

    val n = 64
    // layers must be same length
    val input = Layer(n = n, decay = -1.0, tau = 5.0, name = "x")
    val lambda = LambdaLayer(n = n, f = Math.pow(_, 2.0), name = "x^2")

    input --> lambda

    // set custom color
    input.ui.gradient = Colorf.hsb(40.0)
    lambda.ui.gradient = Colorf.hsb(80.0)

    // layers each have own plot by default, so replace with custom plot
    val plot = Plot(input, lambda)
    plot.range = YRange(-5.0, 5.0)
    plot.showZeroLine = true
    plot.height = 400
    plot.center()

    val i = n / 4
    tracked ++= Seq(input.y(i), lambda.y(i))

    Cos(y = input.y(i), on = 1 s, period = 3 s, scale = 2.0)
  }

}

/* todo: put in manual

Parameters can be modified by
1.  Clicking on the upper left menu button.
2.  Clicking on the "Param" tab in the sidebar
3.  Moving the slider (restricts to min/max) or directly editing in the table (doesn't check min/max)


You can rotate//move meshes by touch/drag mouse on the mesh window.
Zoom mesh by pinch gesture or by "CTRL" + mouse scroll or touch scroll
If not responding, make sure the application is in focus by clicking on title bar
Can double click/tap mesh window to reset

You can use the skin editor, "Skin" tab in the sidebar, to customize skins.
After editing some properties, hit the "Refresh skin" button

 */
