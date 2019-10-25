package memnets.core.impl

import memnets.core._
import memnets.linalg.WMatrix
import memnets.ml._
import memnets.model._
import memnets.ui._

import scala.collection.mutable._

private[core] class ModelImpl(val builder: ModelBuilder, val config: ModelConfig) extends BuiltModel with Logging {
  // hints override config for doubles
  val numberType = config.numberTypeWithHints()
  private val _system = DynamicSystem(WMatrix(numberType = numberType))
  _system.tau = config.tauWithHints()
  _system.game = builder.modelType == ModelType.Game // do b4 body...
  name = builder.name // set after in case model swapped in body

  logger.debug(s"numberType: $numberType")
  logger.debug(f"tau: ${system.tau}%.1f")

  private var _engineListener: EngineListener = NULL_ENGINE_LISTENER
  private var _phasePlot: PhasePlot = _
  private var _usesData = false
  private var _usesFile = false
  private var _usesSizeScale = false

  val controls = ArrayBuffer[GameControl]()
  val equation = ArrayBuffer[Y]()
  val tracked = ArrayBuffer[Trackable]()
  val skins = ListBuffer[SkinType]()
  val trials = ArrayBuffer[Trial]()

  /** useful for testing final results or for automating trial */
  var validator: TickListener = NULL_TICK_LISTENER

  implicit def system: DynamicSystem = _system
  implicit def thisModel: Model = this
  implicit def lastTrial: Trial = {
    if (trials.isEmpty) Trial()
    trials.last
  }
  implicit def phasePlot: PhasePlot = {
    if (null == _phasePlot) _phasePlot = new PhasePlot()(system)
    _phasePlot
  }
  implicit def skinFactory: SF = config.skinFactory
  implicit def workingDir = config.workingDir

  /** only valid in a ticklistener */
  def currentTrial = trials(system.now.trialIndex)
  def fileHint(default: String): String = {
    _usesFile = true
    config.fileHint.getOrElseP(default)
  }
  def sizeHint(default: Int = 8, min: Int = 2): Int = {
    _usesSizeScale = true
    val size = Math.max(min, default * config.sizeScale).toInt
    logger.debug(s"sizeHint: size = $size")
    size
  }
  def loadData(): Data = {
    _usesData = true
    config.data.getOrElse {
      var ds = DataSources.dataSourcesModel.getSelectedItem
      if (ds == null)
        ds = DataSources.dataSourcesModel.getItems.head
      ds.data()
    }
  }
  def usesData = _usesData
  def usesFile = _usesFile
  def usesSizeScale = _usesSizeScale

  def logr = logger
  def onEvent: EngineListener = _engineListener
  def onEvent_=(e: EngineListener) = {
    if (e != null)
      _engineListener = e
  }
  def skin: Option[SkinType] = skins.headOption

  /** NOTE: this will ADD to list, not replace anything.  wanted simple script call */
  def skin_=(sk: SkinType): Unit = {
    if (null != sk)
      skins += sk
    else
      logger.warn("null skin")
  }

  def destroy(): Unit = {
    trials.clear()
    skins.clear()
    _engineListener = NULL_ENGINE_LISTENER
    validator = NULL_TICK_LISTENER

    controls.clear()
    equation.clear()
    tracked.clear()
  }
  def gameLost(): Unit = currentTrial.forceLose = true
}
