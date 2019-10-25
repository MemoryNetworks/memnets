package memnets.model

import memnets.core._

import scala.beans.BeanProperty
import scala.collection.mutable._

/**
 * NOTE: only supports one listener of each type (init, done, reset, ...)
 * winScore is based only on reward for goals.  any bonus should be tacked on after win.
 */
class Trial private[memnets] (val model: Model, val index: Int) extends SimTrial with Logging {
  type T = Signal
  import TrialState._
  implicit def lastGoals: Goals = {
    if (_goals.isEmpty) addGoals(new GoalGroup())
    _goals.last
  }
  @BeanProperty var name: String = EMPTY_STRING
  @BeanProperty var score: Int = 0
  @BeanProperty var bonus: Int = 0
  @BeanProperty var timeBonus: Int = 0
  @BeanProperty var state: TrialState = Init
  @BeanProperty var time: Int = 10000
  @BeanProperty var finishTime: Int = 0
  @BeanProperty var winScore: Int = 0
  @BeanProperty var forceLose: Boolean = false

  private val _goals = ListBuffer[Goals]()
  private[memnets] val _inputs = ArrayBuffer[Signal]()
  private var _activeGoals: Option[Goals] = None
  private var _onDone: Procedure = NULL_PROCEDURE
  private var _onInit: Procedure = NULL_PROCEDURE
  private var _onReset: Procedure = NULL_PROCEDURE

  def addGoals(gg: Goals): Unit = { _goals += gg }
  def activeGoals = _activeGoals
  def activeGoals_=(ag: Goals): Unit = (_activeGoals = Option(ag))
  def description = name ? {
    val i = index + 1
    if (model.system.game)
      s"LEVEL $i"
    else
      s"Trial $i"
  }
  def done(): Unit = { _onDone.body() }

  /**
   * cancels default game logic for this trial/level
   * you must add own listener to score and change state based on own rules
   */
  def findRealTime(elem: Element): Option[RealTime] = {
    var rez: Option[RealTime] = None
    if (elem != null) {
      val opt = _inputs.find(x => x.on != 0 && x.element == elem && x.isUser)
      if (opt.isDefined)
        rez = Option(opt.get.asInstanceOf[RealTime])
    }
    rez
  }
  def goals: Seq[Goals] = _goals
  def ics: Iterator[Signal] = _inputs.iterator.filter(x => x.on == 0 && x.isActive)
  def init(): Unit = {
    _onInit.body()
    state = Playing
  }
  def inputs: scala.collection.IndexedSeq[Signal] = _inputs
  def isPlaying: Boolean = state == Playing

  /**
   * called only once when a trial is first set on a model before the first onReset
   * put heavyweight initialization code here
   */
  def onInit = _onInit
  def onInit_=(f: => Any): Unit = _onInit = new ProcedureImpl(f)
  def setOnInit(f: Procedure): Unit = _onInit = f

  /** called when a new trial replaces this one.  if no new trial on current model, not called */
  def onDone = _onDone
  def onDone_=(f: => Any): Unit = _onDone = new ProcedureImpl(f)
  def setOnDone(f: Procedure): Unit = _onDone = f

  /** called each time a trial begins a run */
  def onReset: Procedure = _onReset
  def onReset_=(f: => Any): Unit = _onReset = new ProcedureImpl(f)
  def setOnReset(f: Procedure): Unit = _onReset = f

  def reset(): Unit = {
    state = Reset
    for (rt <- _inputs.flatMap(_.as[RealTime])) rt.delete
    for (g <- _goals) g.reset() // progress 0, success = false
    if (winScore == 0) {
      winScore = _goals.map(_.reward).sum
      logger.trace(s"winScore not set, so using sum: $winScore")
    }
    activeGoals = _goals.headOption.getOrElse(null)
    score = 0
    bonus = 0
    timeBonus = 0
    finishTime = 0
    _onReset.body()
    state = Playing
  }
  def ticks: Int = time

  def getInputCount: Int = inputs.length
  def getDescription: String = description
  def getTimeText: String = {
    if (time > 0)
//      f"${time/3600}%02d:${(time % 3600) / 60}%02d:${(time % 3600) % 60}%02d"
      f"${time / 3600}%02d:${(time % 3600) / 60}%02d"
    else
      "no limit"
  }
  override def toString = s"Trial[i= $index, name= $name, time= $time]"
}
