package memnets.core

import memnets.model._
import memnets.ui._

import scala.collection.mutable._
import scala.reflect.ClassTag

abstract class ModelUIBase[Z, S <: Skin[Z, _ <: AnyRef]: ClassTag] extends ModelUI with Logging {
  type T = TickableUI[Z]
  protected val _clazz = implicitly[ClassTag[S]].runtimeClass
  protected val _tickables = ArrayBuffer[T]()
  protected var _dirty = false
  protected var _skin: S = _
  private var _engine: Engine = _
  private var _model: BuiltModel = _
  private var _trial: Trial = _

  def engine = _engine
  def engine_=(engine: Engine): Unit = _engine = engine

  def add(elem: Element): Unit = {
    val ui = _skin.create(elem)
    if (ui.isDefined) {
      val t = ui.get
      _tickables += t
      // always set here for find, remove, add
      t.element = elem
      t.init()
      addHelper(t)
    }
  }
  def buildNetwork(doNetBuilt: Boolean = true): Unit = {
    if (null != _skin) {
      _skin.layout(system) // crucial for colors and layout...
      for (t <- _skin.createSystem(system)) {
        _tickables += t
        // don't force this
        // t.data = model
        t.init()
        addHelper(t)
      }
      for (layer <- system.layers)
        add(layer)
      for (extra <- system.elements)
        add(extra)
      for (f <- system.sparse.funcs)
        add(f)
      for (tr <- system.triggers)
        add(tr)
      // adding variables last so on top
      if (system.variablesShown < _skin.sparseLayer) {
        for (n <- system.variables)
          add(n)
      } else if (!skin.hasSparseViz(system)) {
        system.sparse.ui.numericalType = _skin.sparseType
        add(system.sparse)
      }

      _dirty = true
      if (doNetBuilt)
        _skin.systemBuilt()
    }
  }
  def dirty = _dirty
  def find(elem: Element) = _tickables.find(_.element == elem)
  def flashTitle(): Unit = {}
  def updateScore(): Unit = {}
  def isValid(skin: SkinType): Boolean = _clazz.isInstance(skin)
  def model = _model
  def model_=(m: BuiltModel): Unit = {
    logger.debug("model: " + m)
    _model = m
  }

  /** base impl has no timers/animations, so just use now */
  def playGameIntro(): Unit = {
    logger.debug("playIntro: " + system.name)
    publish(new GameIntroOverEvent())
  }
  def playGameOver(win: Boolean): Unit = {}
  def rebuild(): Unit = {
    logger.debug("rebuild")
    // NOTE : this is crucial to clear out b/c engine will likely set bogus Skin at first
    for (t <- _tickables)
      t.destroy
    _tickables.clear
    _skin.init(system)
    buildNetwork()
  }
  def remove(elem: Element): Unit = {
    skin.remove(elem)
    for (t <- _tickables.find(_.element == elem))
      removeT(t)
  }
  def removeT(t: T): Unit = {
    logger.debug(s"remove : $t")
    _tickables -= t
    t.destroy
  }
  def reset(fullReset: Boolean): Unit = {
    logger.debug(s"sysBase reset(fullReset = $fullReset)")
    if (fullReset) {
      val toDel = new ListBuffer[T]()
      for (t <- _tickables) {
        if (t.permanent)
          t.reset
        else
          toDel += t
      }
      for (t <- toDel)
        removeT(t)
    }
    _dirty = true
  }
  def setFPS(fps: Int): Unit = { logger.debug("fps :" + fps) }
  def skin: SkinType = _skin
  def skin_=(s: SkinType): Unit = {
    assert(_model != null, "must set model before skin")
    if (isValid(s)) {
      _skin = s.asInstanceOf[S]
      rebuild()
    } else
      logger.warn(s"bad skin: $s")
  }
  def skinTyped: S = _skin
  def stopAllAnimations(): Unit = {}
  def tick(te: Tick): Unit = {
    val n = _tickables.length
    var i = 0
    while (i < n) {
      _tickables(i).tick(te)
      i += 1
    }
    _dirty = false
  }
  def tickables: IndexedSeq[T] = _tickables
  def trial = _trial
  def trial_=(t: Trial): Unit = {
    require(t != null)
    if (_trial != t) {
      logger.trace("new trial detected")
      _trial = t
    }
  }
  protected def addHelper(tfx: T): Unit
  protected def markDirty(): Unit = { _dirty = true }
}
