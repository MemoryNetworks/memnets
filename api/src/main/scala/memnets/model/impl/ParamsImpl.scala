package memnets.model.impl

import java.beans.PropertyChangeListener

import memnets.linalg.W
import memnets.model._
import memnets.utils._

import scala.collection.mutable.ArrayBuffer

private final class ParamsImpl extends Params with Logging {
  private val _ties = ArrayBuffer[ParamImpl]()
  var unique: Boolean = true

  object all extends IndexedSeq[Param] {
    def apply(i: Int): Param = _ties(i)
    def length = _ties.size
  }
  def remove(t: Param) = {
    require(t != null)
    logger.debug(s"${t}")
    val ti = t.asInstanceOf[ParamImpl]
    _ties -= ti
  }
  protected def createHelper(count: Option[Int], name: String, system: Boolean)(implicit sys: DynamicSystem): Param = {
    val t = new ParamImpl(_ties.size + 1, count.filter(_ > 0).map(x => name + (x + 1)).getOrElse(name))
    t.system = system
    t.baseName = name
    logger.debug(s"${t}")
    _ties += t
    val sub = t.onChange { (_, newW) =>
      val infoOpt = _tieToInfo.get(t)
      if (infoOpt.isDefined) {
        val info = infoOpt.get
        // params can be modified a lot by sliders (expensive), so check change against last modifyW
        val lastW = info._3
        val isZero = newW ~ 0.0
        if (isZero || Math.abs(newW - lastW.value) >= t.precision) {
          logger.debug(f"tied ${t.name} w = $newW%.3f")
          sys.sparse.modifyWs(info._1, newW)
          lastW.value = newW
        }
      }
    }
    _tieToInfo(t) = (new ArrayBuffer[E](), sub, LastW(t.value))
    t
  }
  def create(name: String, system: Boolean = false)(implicit sys: DynamicSystem): Param = {
    if (unique)
      _ties.find(_.name == name).getOrElse(createHelper(None, name, system))
    else {
      val count = _ties.count(_.baseName == name)
      createHelper(Some(count), name, system)
    }
  }
  // should never set this on edge
  val default: Param = new ParamImpl(-1, memnets.model.Param.DEFAULT_NAME) // don't add to tags
  default.max = 1.0
  default.value = 0.0

  def sorted: Iterable[Param] = _ties.filter(!_.system).sortBy(_.name)

  case class LastW(var value: Double)
  type PInfo = (ArrayBuffer[W], Subscribed, LastW)
  private val _tieToInfo = collection.mutable.Map[Param, PInfo]()
  def getWs(p: Param): Iterable[W] = _tieToInfo(p)._1
  protected def findTie(edge: E): Option[(Param, PInfo)] = _tieToInfo.find(_._2._1.find(_ == edge).isDefined)
  def getTie(edge: E): Option[Param] = findTie(edge).map(_._1)
  def setTie(edge: E, optT: Option[Param]): Unit = {
    if (optT.isEmpty) {
      for ((p, info) <- findTie(edge))
        info._1 -= edge
    } else {
      val t = optT.get
      edge.w = t.w
      _tieToInfo(t)._1 += edge
    }
  }
}

private final class ParamImpl(val id: Int, val name: String) extends Param with BeanSupport with Logging {
  var desc = ""
  var baseName = ""
  var roundDigits = 1
  var system = false
  var _func: FuncD = IDENTITY_FUNCD

  private var _max = 1.0
  private var _min = 0.0
  private var _value = 0.0
  private var _precision = 0.01

  def min = _min
  def max = _max
  def precision = _precision
  def value = _value

  def func = _func
  def func_=(f: FuncD): Unit = {
    _func = f
    // force fire so listeners recalc w/ new func
    this._pcs.firePropertyChange("value", 0.95 * value, value)
  }
  def max_=(value: Double): Unit = {
    val oldValue = this._max
    this._max = value
    this._pcs.firePropertyChange("max", oldValue, value)
  }
  def min_=(value: Double): Unit = {
    val oldValue = this._min
    this._min = value
    this._pcs.firePropertyChange("min", oldValue, value)
  }
  def precision_=(value: Double): Unit = {
    val oldValue = this._precision
    if (oldValue != value) {
      this._precision = value
      this._pcs.firePropertyChange("precision", oldValue, value)
    }
  }
  def value_=(value: Double): Unit = {
    val oldValue = this._value
    if (oldValue != value) {
      this._value = value
      this._pcs.firePropertyChange("value", oldValue, value)
    }
  }
  def onChange(listener: (Double, Double) => Unit): Subscribed = {
    val initW = w
    listener(initW, initW) // initialize
    val pe: PropertyChangeListener = pe =>
      listener(func.eval(pe.getOldValue.asInstanceOf[Double]), func.eval(pe.getNewValue.asInstanceOf[Double]))
    addPropertyChangeListener("value", pe)
    val sub = new Subscribed {
      def cancel(): Unit = removePropertyChangeListener("value", pe)
    }
    sub
  }

  /** NOTE: do NOT use Param.w here as func may use model, which may be gone...*/
  override def toString = f"Param[name=$name, value=$value%.1f]"
}
