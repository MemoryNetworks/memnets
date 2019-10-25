package memnets.model

import scala.beans._

object Signal {
  def apply(
      y: Yb,
      on: Int,
      period: Double,
      scale: Double,
      gen: SigGen,
      desc: String = ""
  )(implicit tri: Trial): Signal = {

    val s = new Signal(tgt = y)
    s.on = on
    s.period = period
    s.gen = gen
    s.scale = scale
    s.desc = desc
    tri._inputs += s
    s
  }
}
sealed class Signal private[memnets] (val tgt: Yb) extends Element with SimSignal with Trackable with UIable[YbUI] {
  private var _end: Int = Int.MaxValue
  private var _isDirty = false
  private var _loopCount = 1
  private var _period = 1000.0
  private var _periodInt: Int = _period.toInt
  private var _on = 1
  private var _act: Double = 0.0
  private var _onOff: Procedure = NULL_PROCEDURE
  private[memnets] var _src: UserSource = _

  /** here so subclass can override.  used in ui.loc */
  protected def tgtLoc: Loc = tgt.ui.loc
  @BeanProperty var gen: SigGen = _
  @BeanProperty var scale: Double = 1.0
  @BeanProperty var desc: String = EMPTY_STRING
  @BeanProperty var descOff: String = EMPTY_STRING
  @BooleanBeanProperty var active: Boolean = true

  // Trackable
  final def act: Double = _act
  def isSpiking: Boolean = false
  final def tick(te: Tick): Unit = {
    val t = te.t
    _act =
      if (t >= on && t <= off && active) // useActive here as well
        scale * gen.input((t - on) % _periodInt, _period)
      else
        0.0
  }

  def name = s"Signal for ${tgt.name}"
  def name_=(name: String): Unit = {}
  override def description: String = desc ? f"${gen.name} signal ${scale}%.1f for ${tgt} at t = ${on / 60.0}%.1f s"

  object ui extends YbUI {
    var viz = Viz.Default // don't use tgt b/c could be in hidden layer
    def loc = tgtLoc
    def loc_=(l: Loc): Unit = {}
    def range: YRange = tgt.ui.range
    def scale: Option[Float] = tgt.ui.scale
  }

  def element: Element = tgt
  def isUser: Boolean = false

  /** checks if loopCount = -1 */
  def loop: Boolean = _loopCount == -1
  def loopCount: Int = _loopCount
  def loopCount_=(lc: Int): Unit = {
    _loopCount = lc
    _isDirty = true
  }

  /** period in ticks.  can use f2p extension method */
  def period: Double = _period
  def period_=(p: Double): Unit = {
    // NOTE: don't want to use Integer.MAX_VALUE b/c of overflow with loopCount + on
    _period = if (p <= 0.0) Integer.MAX_VALUE / 32.0 else p
    _periodInt = _period.toInt
    if (_periodInt < 1) _periodInt = 1
    _isDirty = true
  }
  def on: Int = _on
  def on_=(t: Int): Unit = {
    _on = t
    _isDirty = true
  }
  def off: Int = {
    if (_isDirty) {
      _end =
        if (loop) Int.MaxValue
        else {
          val sum: Int = on + (loopCount * period).toInt
          val sumLong: Long = on.toLong + (loopCount.toLong * period.toLong)
          if (sum == sumLong.toInt)
            sum
          else
            Integer.MAX_VALUE
        }
      _isDirty = false
    }
    _end
  }
  def onOff: Procedure = _onOff
  def onOff_=(f: => Any): Unit = { _onOff = new ProcedureImpl(f) }
  def setOnOff(p: Procedure) = { _onOff = p ? NULL_PROCEDURE }
  def src: UserSource = _src

  // Java
  def getLoopCount = loopCount
  def setLoopCount(lc: Int): Unit = { loopCount = lc }
  def getOn = on
  def setOn(t: Int): Unit = { on = t }
  def getOff = off
  def getPeriod = period
  def setPeriod(p: Double): Unit = { period = p }
  def getTarget = tgt.name

  override def equals(other: Any) = other match {
    case a: AnyRef => this eq a
    case default   => false
  }
  override def hashCode = 13 * tgt.id + 17 * on
  override def toString = f"$tgt ${gen.name}@${on.toDouble / 60.0}%.1fs"
}

abstract class RealTime private[memnets] (override val tgt: Yb) extends Signal(tgt) with Logging {
  protected val _rtGen = new RealtimeGen
  override val isUser = true
  override def tgtLoc = src.loc

  _rtGen.add(0, 0) // NOTE: 0 is 1st RELATIVE time entry
  gen = _rtGen

  def add(t: Int, act: Double) = _rtGen.add(t - on, act)
  def delete(): Unit
  def stop(t: Int) = period = t - on
}

object RealTime {
  def apply(
      owner: Element,
      y: Yb,
      on: Int,
      us: UserSource
  )(implicit tri: Trial): RealTime = {
    object rt extends RealTime(y) {
      override def element: Element = owner
      def delete() = {
        if (_rtGen.notrec) {
          logger.debug(s"deleting realtime sig : $src")
          tri._inputs -= this
        }
      }
    }
    rt._src = us
    rt.on = on
    rt.period = 500000
    tri._inputs += rt
    rt
  }
}
