package memnets.core

import memnets.linalg.NumberType
import memnets.ml._
import memnets.model._
import memnets.ui.{NullSkin, SF}
import memnets.utils._

import scala.beans._

object ModelConfig {
  @BeanProperty var workingDirDefault = WorkingDir(".")

  /** java helper */
  def create(): ModelConfig = { apply() }
  def create(numberType: NumberType): ModelConfig = { apply(numberType = numberType) }
  def apply(
      numberType: NumberType = NumberType.Doubles,
      tau: Double = DynamicSystem.TAU_DEFAULT,
      customProvider: SimProvider = null
  ): ModelConfig = {
    val cfg = new ModelConfig
    cfg.numType = numberType
    cfg.tau = tau
    cfg.customProvider = customProvider
    // Ode45 not worthwhile for floats...
    cfg.method = if (numberType == NumberType.Doubles) OdeMethod.Ode45 else OdeMethod.Ode23
    cfg
  }
}

/**
 * holds config properties and helper methods for builder
 * some noted properties should only be called before a builder script
 */
class ModelConfig extends BeanSupport with Logging {

  /** optional file or dir if model needs it  */
  private var _fileHint: Option[String] = None
  private var _workingDir: Option[WorkingDir] = None
  private var _gpuSyncPolicy = GpuSyncPolicy.Always
  private var _gpuSyncCustom: GpuSync = _
  private var _gpuSync: GpuSync = AlwaysGpuSync
  private var _gpuSyncTicks = 8
  private var _skinFactory: SF = () => NullSkin
  private var _customProvider: Option[SimProvider] = None
  private var _usesFile = false
  private var _usesSizeScale = false
  private var _hints: ModelHints = ModelHints()

  def usesFile: Boolean = _usesFile
  def usesSizeScale: Boolean = _usesSizeScale

  /** not bean property on purpose  set in conjunction with a gpu customProvider */
  var gpuSupport = false

  def customProvider: Option[SimProvider] = _customProvider
  def customProvider_=(provider: SimProvider): Unit = {
    _customProvider = Option(provider)
    customSim = customProvider.isDefined
  }

  def getCustomProvider: Option[SimProvider] = customProvider
  def setCustomProvider(provider: SimProvider): Unit = customProvider = provider

  /**
   * seems that AnimationTimer will immediately call tick again on 1st skip during fast forward
   * has not been tested on Mac/Linux
   */
  @BooleanBeanProperty var slowExtraTick = true
  @BooleanBeanProperty var singleThread = true
  @BooleanBeanProperty var customSim = false
  @BeanProperty var numType = NumberType.Doubles
  @BeanProperty var sizeScale = 1.0
  @BeanProperty var tau = DynamicSystem.TAU_DEFAULT

  /** modification of this before call to useDouble or neuralNet() could be lost */
  @BeanProperty var paraThreshold: Int = Math.pow(2, 20).toInt // ~1.0 mil
  @BeanProperty var method = OdeMethod.Ode45
  @BooleanBeanProperty var useHints = true
  @BeanProperty var data: Option[Data] = None

  def gpuSync = _gpuSync
  def gpuSyncCustom: GpuSync = _gpuSyncCustom
  def gpuSyncCustom_=(gpuSync: GpuSync): Unit = {
    require(gpuSync != null)
    _gpuSyncCustom = gpuSync
    gpuSyncPolicy = GpuSyncPolicy.Custom
  }
  def gpuSyncPolicy: GpuSyncPolicy = _gpuSyncPolicy
  def gpuSyncPolicy_=(policy: GpuSyncPolicy): Unit = {
    _gpuSyncPolicy = policy
    gpuSyncHelper()
  }
  def gpuSyncTicks: Int = _gpuSyncTicks
  def gpuSyncTicks_=(value: Int): Unit = {
    require(value > 0)
    _gpuSyncTicks = value
    gpuSyncHelper()
  }
  def hints: ModelHints = _hints
  def hints_=(value: ModelHints): Unit = {
    require(value != null)
    _hints = value
  }
  def createStepper(sim: Sim): SimStepper = {
    if (singleThread || gpuSupport)
      new SingleThreadStepper(sim)
    else
      new FutureStepper(sim)
  }
  def reset(): Unit = {
    data = None
    sizeScale = 1.0
    _usesSizeScale = false
    _usesFile = false

    // don't reset b/c sim may only have Floats support
    // useHints = true

    // don't change any sim/gpu
  }

  def fileHint: Option[String] = _fileHint
  def fileHint_=(f: String): Unit = {
    _fileHint = if (f != null && f.trim().length > 0) Some(f) else None
  }

  /** used as implicit import by ModelBuilder */
  def workingDir: WorkingDir = _workingDir.getOrElseP(ModelConfig.workingDirDefault)
  def workingDir_=(wd: WorkingDir) = _workingDir = Option(wd)

  def skinFactory: SF = _skinFactory
  def skinFactory_=(sf: SF): Unit = {
    require(sf != null)
    _skinFactory = sf
  }

  def gpuSyncHelper(): Unit = {
    import GpuSyncPolicy._
    gpuSyncPolicy match {
      case Always =>
        _gpuSync = AlwaysGpuSync
      case Manual =>
        _gpuSync = ManualGpuSync
      case Ticks =>
        _gpuSync = TickGpuSync(gpuSyncTicks)
      case Custom =>
        require(_gpuSyncCustom != null)
        _gpuSync = _gpuSyncCustom
    }
  }
  def methodWithHints(): OdeMethod = {
    if (useHints)
      _hints.ode.getOrElseP(method)
    else
      method
  }
  def numberTypeWithHints(): NumberType = {
    if (useHints)
      _hints.numberType.getOrElseP(numType)
    else
      numType
  }
  def tauWithHints(): Double = {
    val tauRez = if (useHints) _hints.tau.getOrElseP(tau) else tau
    if (tauRez > 0.0) tauRez else DynamicSystem.TAU_DEFAULT
  }
  def simProvider: SimProvider = {
    if (customSim)
      _customProvider.getOrElse(Sim.apply)
    else
      Sim.apply
  }

  // Java
  def setSkinFactory(sf: SF): Unit = skinFactory = sf
  def getFileHint: String = fileHint.getOrElse("")
  def setFileHint(value: String): Unit = { fileHint = value }
  def getGpuSync = gpuSync
  def getGpuSyncPolicy = gpuSyncPolicy
  def setGpuSyncPolicy(value: GpuSyncPolicy): Unit = gpuSyncPolicy = value
  def getGpuSyncTicks = gpuSyncTicks
  def setGpuSyncTicks(value: Int): Unit = gpuSyncTicks = value
  def getMethodHint = _hints.ode
  def getNumTypeHint = _hints.numberType
  def getTauHint = _hints.tau

  def getWorkingDir: String = workingDir.dir
  def setWorkingDir(wd: String): Unit = {
    if (wd != null && wd.trim().length > 0 && wd.toFile.isDirectory)
      workingDir = WorkingDir(wd)
    else
      workingDir = null
  }
}

case class TickGpuSync(ticks: Int) extends GpuSync {
  def needsSync(tick: Tick, lay: AbstractLayer): Boolean = tick.t % ticks == 0 || tick.end
}

trait GpuSync {
  def needsSync(tick: Tick, lay: AbstractLayer): Boolean
}
