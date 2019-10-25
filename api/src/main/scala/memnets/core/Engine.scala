package memnets.core

import memnets.model._
import memnets.ui._
import memnets.utils._

import scala.beans.BooleanBeanProperty
import scala.collection.mutable._

trait EngineTask[T] {

  /** any calls that modify UI should use AppUI.runOnUI */
  def call(): T
  def onCompleted(t: T): Unit = {}
}

final class Engine(
    val modelUI: ModelUI,
    val appUI: AppUI
) extends BeanSupport
    with EngineListener
    with Logging {

  private var _listeners = new ArrayBuffer[EngineListener](initialSize = 2)

  modelUI.engine = this
  addEngineListener(this)
  addEngineListener(appUI)

  def publish(event: EngineEvent): Unit = {
    val len = _listeners.length
    var i = 0
    while (i < len) {
      _listeners(i).process(event)
      i += 1
    }
  }
  def addEngineListener(listener: EngineListener): Subscribed = {
    _listeners += listener
    () =>
      _listeners -= listener
  }

  val paramsModel = new SelectionModel[Param]()
  val trialsModel = new SelectionModel[Trial]()
  val inputModel = new SelectionModel[Signal]()
  val skinsModel = new SelectionModel[SkinType]()

  // NOTE : can only use @BeanProperty/Boolean if Engine doesn't modify itself
  @BooleanBeanProperty var loop = false
  @BooleanBeanProperty var useValidator = false

  private var _builderChanging = false
  private var _disabled = false
  private var _error = false
  private var _fps = 0
  private var _lastNanoTime, _nanoTime: Long = _
  private var _playing = false
  private var _simStepper: SimStepper = _
  private var _simSteps = 1
  private var _speedMod = 1
  private var _speedModEff = 1
  private val _speedMods = Array(-60, -30, -15, -8, -4, -2, 1, 2, 4)
  private var _speedCount = 0
  // TODO: 2.13 complains here.  look at alternatives
  private val _timeToSigs = new HashMap[Int, Set[SignalEvent]] with MultiMap[Int, SignalEvent]
  private var _trialChanging = false

  private object _gh extends GoalHandler {
    def goalGroupOver(grp: Goals): Unit = {
      publish(GoalsEvent(grp, start = false))
      val i = trial.goals.indexOf(grp) + 1
      if (i < trial.goals.length) {
        val active = trial.goals(i)
        trial.activeGoals = active
        publish(GoalsEvent(active))
      } else
        gameLost()
    }
    def goalOver(g: Goal): Unit = publish(GoalEvent(g, start = false))
    def goalStart(g: Goal): Unit = publish(GoalEvent(g))
  }

  trialsModel.onSelection { t =>
    logger.debug(s"trial changed : $t")
    // can get null when set items on trialsModel
    if (!_builderChanging && null != t) {
      _trialChanging = true
      appUI.task("Setting trial: " + t.description) { () =>
        setTrialHelper(t)
        appUI.runOnUI {
          modelUI.rebuild()
          publish(ResetEvent(t))
          _trialChanging = false
        }
      }
    } else
      disabled = true
  }
  skinsModel.onSelectionChange { (prior, newSkin) =>
    if (!_trialChanging && newSkin != null && prior != newSkin) {
      appUI.task("Building Skin...") { () =>
        appUI.runOnUI {
          modelUI.skin = newSkin
          prior match {
            case fullSkin: FullScene[_, _] =>
              skinsModel.removeItem(fullSkin)
            case default =>
          }
        }
      }
    }
  }
  def builderChanging: Boolean = _builderChanging
  def model: BuiltModel = modelUI.model
  def setBuilder(builder: ModelBuilder, resetCfg: Boolean = false): Unit = {
    logger.debug(s"builder = $builder")
    appUI.task("Building model...") { () =>
      logger.debug("builder task running")
      _builderChanging = true
      if (resetCfg) {
        appUI.config.hints = builder.hints
        appUI.config.reset()
      }
      _builderChanging = true
      cleanUp(postMsg = true)
      progress("Building model...")
      val newModel = builder.build(appUI.config)
      progress("Building simulation....")
      val newSim = newModel.buildSim(doHeadTrial = false)
      _simStepper = appUI.config.createStepper(newSim)
      trialsModel.setItems(newModel.trials)
      // always set model before skin
      modelUI.model = newModel
      val validSkins = newModel.skins.filter(modelUI.isValid)
      // always add after builder skins
      if (validSkins.isEmpty || newModel.useDefaultSkins) {
        for (skin <- modelUI.defaultSkins if skin.isSuitable(newModel.system))
          validSkins += skin
      }
      skinsModel.setItems(validSkins)
      paramsModel.setItems(newModel.system.params.sorted)
      progress("Building skin....")
      appUI.runOnUI {
        paramsModel.selectFirst()
        _trialChanging = true
        trialsModel.selectFirst()
        setTrialHelper(trial)
        skinsModel.selectFirst()
        // needs modelUI.trial set
        modelUI.skin = skinsModel.getSelectedItem
        publish(ResetEvent(trial))
        publish(BuiltModelEvent(newModel))
        _trialChanging = false
        _builderChanging = false
      }
      newModel
    }
  }
  def changeTrial(next: Boolean): Unit = publish(TrialChangeEvent(next = next))
  def cleanUp(postMsg: Boolean = false): Unit = {
    disabled = true
    logger.debug("engine clean up")
    // stop potential multi-thread first.  will block until last tick done
    if (null != _simStepper) {
      _simStepper.reset()
      _simStepper.sim.destroy()
    }
    modelUI.stopAllAnimations()
    val old = model
    if (null != old) {
      progress("Destroying old model+sim...")
      old.destroy()
    }
    speedMod = 1
  }
  def fireReset(): Unit = publish(ResetEvent(trial))
  def sim: Sim = _simStepper.sim
  def speedMods: Array[Int] = _speedMods
  def trial: Trial = trialsModel.getSelected

  /**
   * NOTE: this method is mostly called from event handlers,
   * so should be in UI thread already
   */
  def process(e: EngineEvent): Unit = {
    try {
      logger.trace("evt : " + e)
      if (model != null) model.onEvent.process(e)
      e match {
        case RealSampleEvent(rt, act) =>
          logger.trace("user drag")
          rt.add(sim.t, act)
        case RealStartEvent(y, elem, src, init, touch) =>
          logger.debug(s"user start at ${sim.t}")
          val rt = RealTime(owner = elem, y = y, on = sim.t + 1, us = src)(trial)
          for (init <- init) rt.add(1, init)
          _timeToSigs.addBinding(rt.on, SignalEvent(signal = rt, on = true))
        case RealEndEvent(us, touch) =>
          logger.debug("user end")
          // todo : issue here if Sim using Future...
          us.stop(sim.t)
          modelUI.remove(us)
          _timeToSigs.removeBinding(us.off, SignalEvent(signal = us, on = false))
          // delete last to avoid adding twice...
          us.delete()
          logger.trace(s"num realtimes = ${trial.inputs.flatMap(_.as[RealTime]).size}")
        case SignalEvent(sig, on) =>
          if (on) {
            logger.trace(s"sig on : $sig")
            modelUI.add(sig)
          } else {
            logger.trace(s"sig off : $sig")
            sig.onOff.body()
            modelUI.remove(sig)
          }
        case ResetEvent(tri) =>
          if (!_trialChanging)
            stop()
          reset()
        case GoalEvent(g, start) =>
          if (start) {
            g.startTick = sim.nowTick.t
            modelUI.add(g)
          } else {
            modelUI.remove(g)
            import TrialState._
            if (g.isCompleted) {
              g.completedTick = sim.nowTick.t
              trial.score = trial.score + g.reward
              if (trial.state == Playing) {
                if (trial.score >= trial.winScore) {
                  trial.finishTime = sim.nowTick.t
                  trial.bonus = trial.goals.map(_.bonusCalc(trial)).sum
                  val timeLeft = trial.time - trial.finishTime
                  val halfSec = (30 s)
                  trial.timeBonus = timeLeft / halfSec
                  publish(GameEndEvent(sim.nowTick.t, win = true))
                } else if (trial.score < 0 || (trial.goals.map(_.goalsLeft).sum + trial.score) < trial.winScore)
                  gameLost()

                modelUI.updateScore()
              }
            }
            if (trial.state == Playing) {
              for (ag <- trial.activeGoals)
                ag.next(_gh)
            }
          }
        case pl: TogglePlayEvent =>
          if (isPlaying)
            msg("System paused by user")
          else
            msg("System resumed by user")
          setPlaying(!isPlaying)
        case TrialChangeEvent(next) =>
          if (next)
            trialsModel.selectNext()
          else
            trialsModel.selectPrevious()
        case gie: GameIntroOverEvent =>
          disabled = false
        case GameEndEvent(t, win) =>
          // NOTE: difference here that trial.state updated AFTER event fired
          trial.state = if (win) TrialState.Won else TrialState.Lost
          logger.trace(s"score = ${trial.score}, win = ${win}")
          disabled = true
          modelUI.playGameOver(win)
        case SpeedEvent(inc) =>
          var i = 0
          val speedIndex = _speedMods.indexOf(_speedMod)
          if (inc) {
            i = speedIndex + 1
            if (i == _speedMods.length) i = _speedMods.length - 1
          } else {
            i = speedIndex - 1
            if (i < 0) i = 0
          }
          // call setter for beansupport event
          speedMod = _speedMods(i)
          logger.debug(s"speed $inc: mod = ${_speedMod}, modEff = ${_speedModEff}")

          // really don't like this here, but FF on non mod Tick.t will screw up chart/listeners
          if (speedMod > 1 && (sim.t % speedMod != 0)) {
            logger.debug("advancing sim")
            _speedCount = -1
            var zeroMod = false
            while (!zeroMod) {
              sim.step()
              zeroMod = sim.t % speedMod == 0
            }
          }
        case FlashTitleEvent(repeat) =>
          logger.debug("flash title")
          for (i <- 0 until repeat)
            modelUI.flashTitle()
        case default =>
      }
    } catch {
      case th: Throwable =>
        logger.error("catch error")
        publish(ErrorEvent("processing error", th))
    }
  }

  def tick(now: Long): Unit = {
    try {
      if (_playing && !_disabled) {
        _speedCount += 1
        if (_speedCount > 0 && (_speedCount % _speedModEff == 0)) {
          _speedCount = if (_speedModEff == 1) 0 else if (appUI.config.slowExtraTick) -1 else 0

          _nanoTime = System.nanoTime
          if (_nanoTime > _lastNanoTime + 1000000000) {
            modelUI.setFPS(_fps)
            _fps = 0
            _lastNanoTime = _nanoTime
          }
          _simStepper.step(_simSteps)
          _fps += _simSteps
          if (trial.forceLose)
            gameLost()

          val tick = sim.nowTick
          val t = tick.t
          if (_timeToSigs.contains(t)) {
            for (se <- _timeToSigs(t))
              publish(se)
          }
          if (trial.activeGoals.isDefined)
            trial.activeGoals.get.tick(tick, _gh)

          modelUI.tick(tick)
          appUI.tick(tick)
          if (useValidator)
            model.validator.tick(tick)

          if (tick.end) {
            if (sim.system.game)
              gameLost()
            else {
              if (loop)
                fireReset()
              else {
                disabled = true
                trial.state = TrialState.Done
                publish(TrialDoneEvent(trial))
              }
            }
          } else
            _simStepper.next(isPlaying)
        }
      }
//      else if (sysUI.dirty)
//        sysUI.tick(sim.nowTick)
    } catch {
      case th: Throwable =>
        publish(ErrorEvent("engine tick error", th))
    }
  }
  def error: Boolean = _error
  def error_=(value: Boolean): Unit = {
    logger.error("error: " + value)
    this.synchronized {
      val oldValue = this._error
      if (oldValue != value) {
        this._error = value
        this._pcs.firePropertyChange("error", oldValue, value)
        if (error) disabled = true
      }
    }
  }

  /** user modifiable  */
  def playing: Boolean = _playing
  def playing_=(value: Boolean): Unit = {
    logger.trace("playing: " + value)
    this.synchronized {
      val oldValue = this._playing
      if (oldValue != value) {
        this._playing = value
        this._pcs.firePropertyChange("playing", oldValue, value)
      }
    }
  }
  def disabled: Boolean = _disabled
  def disabled_=(value: Boolean): Unit = {
    logger.trace("disabled: " + value)
    this.synchronized {
      val oldValue = this._disabled
      if (oldValue != value) {
        if (!error || value) {
          this._disabled = value
          this._pcs.firePropertyChange("disabled", oldValue, value)
          playing = !value
        } else if (error)
          logger.warn("when error, disable = false ignored")
      }
    }
  }
  def speedMod: Int = _speedMod
  def speedMod_=(value: Int): Unit = {
    require(_speedMods.indexOf(value) != -1, "invalid speed")
    val oldValue = this._speedMod
    this._speedMod = value
    _speedCount = 0
    // defaults
    _speedModEff = 1
    _simSteps = 1
    if (value < 0) {
      _speedModEff = -value
    } else
      _simSteps = value
    this._pcs.firePropertyChange("speedMod", oldValue, value)
  }

  private def gameLost(): Unit = publish(GameEndEvent(t = sim.nowTick.t, win = false))
  private def progress(msg: String): Unit = { publish(ProgressEvent(msg)) }
  private def msg(msg: String): Unit = { publish(MessageEvent(msg)) }
  private def reset(): Unit = {
    sim.reset()
    _timeToSigs.clear()
    for (in <- trial.inputs if in.on != 0) {
      _timeToSigs.addBinding(in.on, SignalEvent(signal = in, on = true))
      _timeToSigs.addBinding(in.off, SignalEvent(signal = in, on = false))
    }
    appUI.runOnUI {
      modelUI.reset(fullReset = true)
      for (grp <- trial.activeGoals) {
        publish(GoalsEvent(grp, start = true))
        for (g <- grp.startGoals)
          publish(GoalEvent(goal = g, start = true))
      }
      modelUI.playGameIntro()
    }
  }
  private def setTrialHelper(t: Trial): Unit = {
    stop()
    _simStepper.sim.trial = t
    inputModel.setItems(t.inputs)
    modelUI.trial = t
    publish(TrialEvent(t))
  }
  private def stop(): Unit = {
    disabled = true
    _simStepper.reset() // will wait if threaded old sim
  }

  // Java
  def isDisabled: Boolean = disabled
  def setDisabled(value: Boolean): Unit = disabled = value
  def isError: Boolean = error
  def setError(value: Boolean): Unit = error = value
  def isPlaying: Boolean = playing
  def setPlaying(value: Boolean): Unit = playing = value
  def getSpeedMod: Int = speedMod
  def setSpeedMod(value: Int): Unit = speedMod = value
}
