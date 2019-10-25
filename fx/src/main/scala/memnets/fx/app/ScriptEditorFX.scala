package memnets.fx.app

import javax.script.ScriptException
import memnets.core._
import memnets.fx._
import memnets.fx.utils.JTaskSupport._
import memnets.fx.utils._
import memnets.linalg._
import memnets.model._
import scalafx.Includes._
import scalafx.scene.input.KeyCombination

trait ScriptEditorOwner extends EditorOwner {
  def showScriptMsg(msg: String): Unit
  def preScriptBuild(): Unit
  def fxUsesModules: Boolean
  def taskSupport: JTaskSupport
  def config: ModelConfig
  def setBuilder(b: BldType, resetCfg: Boolean = false): Unit
}

class ScriptEditorFX(owner: ScriptEditorOwner) extends CodeEditorFX(owner, DslEditorDef) {
  protected var _scriptBuilder: Option[ScriptBuilder] = None

  // editor action
  val importCsvAction = ActionP("Import Csv", '\uf019', "Import sparse.variables from matrix in .csv file") {
    try {
      for (file <- owner.open(List("*.csv"), "csv files")) {
        // read in to validate and see how big
        // no sense in generating script if doesn't work
        // might be issue with doing this work twice if large matrix, but not worrying about it
        val matrix = breeze.linalg.csvread(file = file)
        val sys = DynamicSystem(WMatrix(numberType = NumberType.Floats))
        val variables = sys.sparse.fromMatrix(matrix)
        val sb = new StringBuilder
        sb.append("// auto-generated script.  run to import\n\n")
        sb.append("// this line tries to read matrix from file\n")
        sb.append(s"""val matrix = csvread(file = raw"${file.getAbsoluteFile}")\n\n""")
        sb.append("// this line tries to create model.sparse from matrix\n")
        sb.append(s"val variables = model.sparse.fromMatrix(matrix)\n\n")
        sb.append(s"// the importer found variable.length = ${variables.length} \n")
        sb.append("// add your own trials, inputs, skins, ... below using variables(i) \n\n")
        text = sb.toString()
        owner.showMsg("created script to import sparse.variables from csv: " + file.getAbsolutePath)
      }
    } catch {
      case th: Throwable =>
        val msg = "could not import Sparse.variables from file"
        logger.error(msg, th)
        owner.showMsg(msg)
    }
  }
  val exportCsvAction = ActionP("Export Csv", '\uf093', "Export sparse.variables to matrix in .csv file") {
    try {
      for (file <- owner.save(List("*.csv"), "csv files")) {
        val matrix = owner.model.system.sparse.toMatrix()
        breeze.linalg.csvwrite(file, matrix)
        owner.showMsg("exported sparse.variables to: " + file.getAbsolutePath)
      }
    } catch {
      case th: Throwable =>
        val msg = "could not export Sparse.variables to matrix"
        logger.error(msg, th)
        owner.showMsg(msg)
    }
  }

  val runScriptAction = ActionP("Run", '\uf0e7') {
    owner.preScriptBuild()
    def runScriptHelper(): Unit = {
      owner.showScriptMsg("Compiling")
      for (sb <- _scriptBuilder)
        owner.setBuilder(sb(text), resetCfg = true)
    }
    if (_scriptBuilder.isEmpty) {
      owner.showMsg("Compiler initialization only happens the 1st time but may take several seconds")
      val msg = "Initializing compiler..."
      owner.showScriptMsg(msg)
      val initTask = TaskP(msg) {
        logger.debug("init compiler")
        // just use FX builder and re-run to clear FX11+ errors
        //        scriptBuilder = Option(if (fxUsesModules) new ScriptBuilder else new ScriptBuilderFX)
        _scriptBuilder = Option(new ScriptBuilderWithFX)
        if (owner.fxUsesModules) {
          // nasty hack to attempt to "clear out" bogus JavaFX11+ embedded/module classpath errors
          for (sb <- _scriptBuilder) {
            logger.debug("extra javafx11+ script init")
            val warmupScript =
              """
                |autoReleaseRT = true
                |val e = Y("Exc")
                |val e2 = Y("Exc")
                |val i = Y("Inh")
                |e.ui.loc = Loc()
                |e.ui.color = Color.Aqua.sat(0.8)
                |e2.ui.color = Color.hsb(20,0.8,1.0,1.0)
                |
                |tracked ++= Seq(e, e2)
                |Phase3D(e, e2, i, scale = 20)
                |phasePlot.scale = 0.8
                |phasePlot.zoom = 1.4
                |phasePlot.onCreateScene = { sc =>
                |  val scene = sc.asInstanceOf[Scene3DFX]
                |  val rad = Pendulum3DFX.RADIUS
                |  val efx = new Pendulum3DFX(e,i,e.ui.colorfx,rad)
                |  efx.translateZ = rad
                |  scene += (efx)
                |}
                |var count = 0
                |system.sparse.onSpike = te => {
                |  count += 1
                |}
                |system.onReset = {
                |  count += 1
                |}
                |skin = Skin { x =>
                |  x.backImageOn = false
                |  x.zoom = 1.5
                |}
                |val freq = Param("f", max = 2.0, init = 0.5, precision = 1e-4)
                |val osc = Osc(freq, freq)
                |Trial(name = "IC by Step", time = 1 m)
                |Step(y = osc, on = 0, scale = 10.0)
                |YGoal(osc.y, 10.0)
                |system.elements += new ElementBase with TickableFX {
                |  def node: Option[Node] = None
                |  def tick(te: Tick): Unit = {
                |    val gc = RenderContextFX.gc
                |    gc.globalAlpha = 0.7
                |  }
                |}
                |val swta = new SoftWTA(n = 2)
                |val pal = OpacityMapFX(4.0f, numColors = 8)
                |skins += new BatterySkinFX {
                |  override def createGrid(g: GridData): Option[UI] = {
                |    val sc3d = new Scene3DFX(loc = g.ui.loc, w = 500, h = 500, zoom = 1.5)
                |    sc3d.sun.translateY = 1000.0
                |    sc3d.sun.color = Color.web("#777")
                |    sc3d += new Box3DFX(grid = g.subGridData(64, 64), colorMap = pal, rad = 6.0)
                |    create3d(sc3d)
                |  }
                |}
                |""".stripMargin
            var passed = false
            for (i <- 0 until 6 if !passed) {
              try {
                val bld = sb.apply(warmupScript)
                bld.build(owner.config)
              } catch {
                case se: ScriptException =>
                  val msg = se.getMessage.take(300)
                  logger.warn(s"init compiler: warm-up run$i: $msg")
              }
              logger.debug("warm-up script parsing passed")
              // make sure to run at least twice
              passed = i > 0
            }
          }
        }
      }
      initTask.onSucceeded = e => {
        logger.debug("compiler good, parse script")
        runScriptHelper()
      }
      initTask.onFailed = e => {
        logger.debug("compiler failed")
        owner.hideEditor(0 ms)
        throw new IllegalStateException(
          "scala compiler failed to load.  no scripts will work",
          new Throwable(
            "make sure EXACT same scala compiler major+minor jar version used to compile app are on the classpath.  default is 2.12.9")
        )
      }
      initTask.start()(owner.taskSupport)
    } else
      runScriptHelper()
  }

  runScriptAction.setAccelerator(KeyCombination("CTRL+R"))
  closeEditorAction.setAccelerator(KeyCombination("CTRL+W"))
  runScriptAction.disabledProperty <== owner.taskSupport.taskRunning || codeArea.hasText.not()

  def postBuild() = {
    val sys = owner.model.system
    exportCsvAction.enabled = sys.variables.length > 1 &&
      sys.sparse.matrix.weights.length > 0 &&
      sys.layers.isEmpty

  }
  def scriptError(se: ScriptException): Unit = {
    runLaterP {
      val (row, col) = _scriptBuilder.get.scriptErrorToPos(se)
      val head = se.getMessage.linesIterator.headOption.get
      codeArea.moveCaretTo(row, col)
      val msg = s"${head}line ${row + 1} at col ${col + 1}"
      owner.showScriptMsg(msg)
      if (se.getMessage.contains("javafx.") && se.getMessage.contains("is missing from the classpath"))
        owner.showMsg(
          "javafx classpath errors may not be fatal, but can't directly call javafx in 11.0+.  try run again to see if error changes.")

      logger.debug("showParseError complete")
    }
  }

}
