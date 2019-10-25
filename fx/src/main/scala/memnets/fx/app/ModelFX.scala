package memnets.fx.app

import memnets.core.Sound._
import memnets.core._
import memnets.fx._
import memnets.fx.fx3d._
import memnets.fx.utils.ActionP
import memnets.model._
import memnets.ui._
import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Pos
import scalafx.scene._
import scalafx.scene.canvas.Canvas
import scalafx.scene.chart.LineChart
import scalafx.scene.control._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object ModelFX {
  val HEADER_H = 150.0
}

class ModelFX extends ModelUIBase[Node, SkinFX] with Logging {
  import ModelFX.HEADER_H

  val showScore = BooleanProperty(false)
  val score = IntegerProperty(0)
  val winScore = IntegerProperty(0)
  val zoom = DoubleProperty(1.0)

  val zoomStdAction = ActionP("Zoom 1.0", '\uf002', "Zoom Standard") { zoomReset() }
  val zoomInAction = ActionP("Zoom in", '\uf00e') { zoom.value *= 1.1 }
  val zoomOutAction = ActionP("Zoom out", '\uf010') { zoom.value *= 0.9 }
  // don't disable b/c centers as well
  // zoomStdAction.disabledProperty <== zoom === 1.0

  // todo: redo w/o scalafx
  protected val _selectedY = ObjectProperty[Y](initialValue = null)
  def selectedY: ReadOnlyObjectProperty[Y] = _selectedY
  val selectedElement = ObjectProperty[Element](initialValue = null)
  selectedElement ==> {
    _ match {
      case lnk: Linkable =>
        _selectedY.value = lnk.src
      case default =>
        _selectedY.value = null
    }
  }

  // issue with loading, so use lazy.  could also make var and put in loadResources...
  protected lazy val _scorePosSound = Sound(SCORE_POS)
  protected lazy val _scoreNegSound = Sound(SCORE_NEG)
  protected lazy val _wonSound = Sound(WON, speed = 0.8)
  protected lazy val _lostSound = Sound(LOST)
  protected val _clock = new SystemClock
  protected val _gameControls = ArrayBuffer[GameControlFX]()
  protected val _animations = collection.mutable.Map[Transition, Node]()
  protected object _elemDPad extends TickableFX {
    val dPad = new DPad()
    var rtOpt: Option[RealTime] = None
    dPad.onActive { active =>
      if (active) {
        val elem = selectedElement.value
        logger.debug("mouse user start")
        val tgt = selectedY.value
        publish(RealStartEvent(y = tgt, elem = elem, src = null))
        rtOpt = trial.findRealTime(elem)
      } else {
        logger.debug("mouse user stop")
        for (rt <- rtOpt) {
          publish(RealEndEvent(rt = rt))
          rtOpt = None
        }
      }
    }
    def node = None
    def tick(tick: Tick): Unit = {
      if (rtOpt.isDefined) {
        val rt = rtOpt.get
        publish(RealSampleEvent(rt, YRange.scale * (dPad.up - dPad.down)))
      }
    }
    override def reset(): Unit = {
      rtOpt = None
    }
  }

  private object _elemPane
      extends Pane(new JPane {
        setManaged(false)
        setWidth(Region.USE_PREF_SIZE)
        setHeight(Region.USE_PREF_SIZE)
        minWidth(Region.USE_PREF_SIZE)
        maxWidth(Region.USE_PREF_SIZE)
        minHeight(Region.USE_PREF_SIZE)
        maxHeight(Region.USE_PREF_SIZE)
      })
  private object _mainCanvas extends Canvas {
    managed = false
    mouseTransparent = true
  }
  private object _mainGroup extends Group {
    managed = false
    autoSizeChildren = false
    scaleX <== zoom
    scaleY <== zoom
  }
  private object _custHeaderElement extends ElementBase
  private val _ui: JStackPane = "model.fxml".loadFXML
  val fx = new StackPane(_ui)
  private val _timePane = new AnchorPane(_ui.findById("timePane"))
  private val _menuPane = new StackPane(_ui.findById("menuPane"))
  private val _menuBtn = new ToggleButton(_menuPane.findById[javafx.scene.control.ToggleButton]("menuBtn"))
  private val _glass = new Pane(_ui.findById("glass"))
  private val _cachedPane = new StackPane(_ui.findById("cachedPane"))
  private val _cachedPaneReflection = _cachedPane.effect.value
  private val _fpsText = _ui.findTextById("fpsText")
  private val _nameText = new Text(_ui.findTextById("nameText"))
  private val _nameEffect = _nameText.effect.value.asInstanceOf[javafx.scene.effect.DropShadow]
  private val _scorePane = new StackPane(_ui.findById("scorePane"))
  private val _scoreLabel = new Text(_ui.findTextById("scoreLabel"))
  private val _scoreText = new Text(_ui.findTextById("scoreText"))
  private val _scoreWinText = new Text(_ui.findTextById("scoreWinText"))
  private val _lineChart = new LineChart[Number, Number](_ui.findById("timeChart"))
  private val _matrixScroll = new ScrollPane(_ui.findById("matrixScroll"))
  private val _headerPane = new AnchorPane(_ui.findById("headerPane"))

  private val _eqFX = new EquationFX(new TextFlow(_matrixScroll.contentProperty.value.asInstanceOf[JTextFlow]))
  // NOTE : this might be moved to ModelBuilder in future
  private val _timeChart = new TimeChart()
  private object _defaultClockFX extends TickableFX {
    val clockPane = new Pane(_ui.findById("clockPane"))
    val timerText = new Text(_ui.findTextById("timerText"))
    val timerArc: javafx.scene.shape.Arc = _ui.findById("timerArc")
    timerArc.fill = null
    timerArc.stroke = Color.Aquamarine
    val fx = clockPane
    val node = None // don't add to ElemGroup, already in scene
    def tick(te: Tick): Unit = {}
    override def init(): Unit = {
      timerText.text <== _clock.text
      timerArc.lengthProperty <== -_clock.degree
      warnSub = _clock.warn ==> { b =>
        if (b && system.game) {
          timerArc.stroke = Color.Crimson
          clockWarn.playFromStart()
        } else {
          clockWarn.stop()
          timerArc.opacity = 1.0
          timerArc.stroke = Color.Aquamarine
        }
      }
      fx.onMouseClicked = e => {
        import TrialState._
        val state = trial.state
        e.clickCount match {
          case 1 if state == Playing =>
            publish(new TogglePlayEvent())
          case 1 if state == Done =>
            publish(new TrialChangeEvent(next = true))
          case 1 if state == Won =>
            publish(TrialChangeEvent(next = true))
          case 1 if state == Lost =>
            publish(ResetEvent(trial))
          case 2 =>
            publish(ResetEvent(trial))
          case default =>
        }
      }
      // pre-load
      _scorePosSound.start(volume = 0.0)
      _scoreNegSound.start(volume = 0.0)
    }
    override def destroy(): Unit = {
      timerText.text.unbind()
      timerArc.lengthProperty.unbind()
      for (s <- warnSub) s.cancel()
    }
    var warnSub: Option[Subscription] = None
    val clockWarn = blinker(timerArc, dur = 480)
    val clockNextLevel = blinker(timerText, dur = 1000)
    clockNextLevel.onFinished = e => {
      timerText.opacity = 1.0
    }
  }
  // animations
  private val _gameLoseAnimation = pauseBlink(
    n = _nameText,
    pauseMs = 800,
    endOpacity = 0.05,
    onStart = {
      //  nameText.fill = Color.Crimson
      _nameText.text = "TRY AGAIN"
      _nameEffect.setColor(Color.Crimson)
      _lostSound.start(0.6)
    }
  )
  private val _gameWinAnimation = pauseBlink(
    n = _nameText,
    pauseMs = 2000,
    endOpacity = 0.05,
    onStart = {
      //  _nameText.fill = Color.Aquamarine
      _nameText.text = "WINNER WINNER!"
      _nameEffect.setColor(Color.Aquamarine)
      _wonSound.start(0.6)
    }
  )
  _gameWinAnimation.onFinished = e => {
    _defaultClockFX.clockNextLevel.playFromStart()
  }
  private val _introTimeline = new Timeline {
    cycleCount = 1
    autoReverse = false
    keyFrames = Seq(
      KeyFrame(time = 0.0 ms, name = "step1", onFinished = e => {
        logger.debug("intro: start")
        _headerPane.visible = false
        for (c <- gameControls) c.fxNode.visible = false
      }),
      KeyFrame(time = 400.0 ms, name = "step2", onFinished = e => {
        logger.trace("intro: playing animation")
        flashTitle()
      }),
      KeyFrame(time = 2.0 s, name = "step3", onFinished = e => {
        logger.trace("intro: controls on")
        _headerPane.visible = true
        for (c <- gameControls) c.fxNode.visible = true
      }),
      KeyFrame(time = 2.05 s, name = "step4")
    )
    onFinished = e => {
      logger.debug("intro: finished")
      publish(new GameIntroOverEvent())
    }
  }
  private val _startDefault = pauseBlink(
    n = _nameText,
    endOpacity = 0.05,
    onStart = {
      _nameText.text.value = model.name.toUpperCase
      _nameText.fill = Color.White
      _nameEffect.setColor(Color.White)
    },
    onSecond = {
      _nameText.text.value = trial.description
    }
  )
  private val _scoreLabelFlash = pauseBlink(n = _scoreLabel, pauseMs = 200, endOpacity = 0.3)
  private var _startAnimation: Option[Animation] = None
  private var _stopAnimation: Option[Animation] = None
  private var _scoreAnimation: Option[Timeline] = None
  private var _doingReset = false
  private var _selectedSub: Option[Subscription] = None
  private var _gameControlSkin: GameControlSkinFX = new GameControlSkinFX {}

  def control(gc: GameControl): Unit = {
    for (ctrl <- _gameControlSkin.control(gc)) {
      _gameControls += ctrl
      ctrl.reset
      val cfx = ctrl.fxNode
      fx.children.add(cfx)
      StackPane.setAlignment(cfx, ctrl.pos)
    }
  }
  def title = _nameText
  def chart = _timeChart
  def defaultSkin: SkinType = SkinFX()
  override def defaultSkins = {
    val skins = ListBuffer(
      defaultSkin,
      BubbleSkinFX(),
      Skin3DFX(),
      new PlotSparseSkinFX(),
      new ChartSkinFX(),
      new GraphSkinFX(),
      BoxSkin3DFX(),
      new CircleSkinFX())
    if (system.elements.find(_.isInstanceOf[Osc]).isDefined)
      skins += PendulumSkin3DFX()
    skins
  }
  override def stopAllAnimations(): Unit = {
    logger.debug("stopping intro")
    _introTimeline.stop()
  }
  override def flashTitle(): Unit = {
    _startAnimation.getOrElseP(_startDefault).playFromStart()
  }
  def gameControls: collection.IndexedSeq[GameControlFX] = _gameControls
  def gameControlSkin = _gameControlSkin
  def gameControlSkin_=(gc: GameControlSkinFX): Unit = { _gameControlSkin = gc }

  override def init(): Unit = {
    super.init()
    fx.children.add(_mainGroup)
    _mainGroup.toFront() // o.w. chart reflection on top of scene
    _menuPane.toFront() // always on top
    fx.id = "system" // exact id used for css styling, client could change
    fx.focusTraversable = false
    Display.onChange { res =>
      fx.prefWidth.value = res.width
      fx.prefHeight.value = res.height
    }
    _elemPane.prefWidth <== fx.prefWidth
    _elemPane.prefHeight <== fx.prefHeight
    fx.onZoom = e => {
      zoom.value = zoom.value * e.zoomFactor
      e.consume()
    }
    fx.onScroll = e => {
      if (e.getTouchCount <= 1) {
        // if don't filter, background scrolls when drag element using touch
        val tgt = e.getTarget
        if (tgt == _elemPane.delegate || tgt == fx.delegate) {
          if (Math.abs(e.getDeltaX) > Math.abs(e.getDeltaY))
            _mainGroup.translateX.value = _mainGroup.translateX.value + e.getDeltaX
          else {
            if (e.isControlDown)
              zoom.value = zoom.value * (1.0 - (e.deltaY / 1000.0))
            else
              _mainGroup.translateY.value = _mainGroup.translateY.value + e.deltaY / 5.0
          }
          e.consume()
        }
      }
    }
    /* double click here turns off signals
    onMouseClicked = e => {
      if (e.getClickCount == 2)
        zoomReset
    }
     */
    // init keyboard
    fx.handleEvent(KeyEvent.Any) { keyHandler(_) }
    _lineChart.onMouseClicked = e => {
      e.clickCount match {
        case 2 =>
          _matrixScroll.visible = true
          _lineChart.visible = false
        case default =>
      }
    }
    _matrixScroll.onMouseClicked = e => {
      e.clickCount match {
        case 2 =>
          _matrixScroll.visible = false
          _lineChart.visible = isTimeChartVisible
          fx.requestFocus() // sometimes chart can steal keyHandler...
        case default =>
      }
    }
    _matrixScroll.visible ==> { b =>
      if (b) _eqFX.draw(model)
    }
    _timePane.focusTraversable = false
    _menuBtn.focusTraversable = false
    _fpsText.setVisible(true)
    _scoreText.text <== Bindings.createStringBinding(() => f"${score.value}%02d", score)
    _scoreWinText.text <== Bindings.createStringBinding(() => f"${winScore.value}%02d", winScore)
    _scorePane.visible <== showScore
  }
  def menuSelected: BooleanProperty = _menuBtn.selected
  override def model_=(m: BuiltModel): Unit = {
    for (sub <- _selectedSub)
      sub.cancel()
    _selectedSub = None

    super.model_=(m)
    fx.userData = m
    _selectedSub = selectedY ==> { selectY(_) }
  }
  override def playGameIntro(): Unit = {
    _introTimeline.rate = if (title.isVisible && _timePane.isVisible) 1.0 else 4.0
    _introTimeline.playFromStart()
  }
  override def playGameOver(win: Boolean): Unit = {
    Sound.stopAll()
    _stopAnimation = if (win) _gameWinAnimation else _gameLoseAnimation
    for (anim <- _stopAnimation) anim.playFromStart()
  }
  override def rebuild(): Unit = {
    logger.debug(s"rebuild")
    assert(system != null, "must have model to rebuild")
    stopEditAnimations()
    for (t <- _tickables)
      t.destroy
    _tickables.clear
    _elemPane.children.clear()
    for {
      ctrl <- gameControls
      cfx <- fx.children.find(_.userData == ctrl.data)
    } fx.children -= cfx
    _gameControls.clear()
    for (cust <- _headerPane.children.find(_.getUserData == _custHeaderElement)) {
      logger.debug("removing old custom header")
      _headerPane.children.remove(cust)
    }
    logger.debug(s"building w/ skin = ${_skin}")
    _skin.init(system)
    _skin.initStyle(fx, _timePane)
    _mainGroup.children = _skin.initCanvas(_elemPane, _mainCanvas)
    _timePane.visible = _skin.topPaneOn
    _lineChart.visible = true
    _matrixScroll.visible = false

    _timeChart.sampling = _skin.chartSampling
    _timeChart.dynamicRange.value = _skin.chartDynamic
    _glass.visible = _skin.chartGlassOn
    _fpsText.setVisible(_skin.fpsOn)
    if (_skin.sparseHints) {
      val plot = system.sparse.ui.plot
      plot.width = _skin.sparseWidth
      plot.height = _skin.sparseHeight
      plot.hideGlass = !_skin.sparseGlass
      plot.useEffect = _skin.sparseEffect
      plot.center()
    }

    zoomReset()
    title.visible = _skin.titleOn
    if (_skin.customClock) {
      _defaultClockFX.fx.visible = false // MemNetUIBase super will add
      add(_clock)
    } else {
      _defaultClockFX.fx.visible = true
      addTickableOnly(_defaultClockFX)
    }

    if (_skin.customPanel.isDefined) {
      logger.debug("custom header")
      _lineChart.visible = false
      _matrixScroll.visible = false
      for {
        cust <- _skin.customPanel
        fx <- cust.node
      } {
        fx.userData = _custHeaderElement
        fx match {
          case p: Region =>
            p.prefHeight.value = HEADER_H
          case default =>
        }
        fx.prefHeight(HEADER_H)
        _headerPane.children.add(fx)
        AnchorPane.setAnchors(fx, 0, 0, 0, 0)
        addTickableOnly(cust)
      }
    } else if (_skin.customTimeChart) {
      _lineChart.visible = false
      add(_timeChart)
    } else {
      _lineChart.visible.value = _skin.chartOn
      if (_skin.chartOn)
        addTickableOnly(new TimeChartFX(_timeChart, _lineChart, _skin.yColorMap))
    }
    _skin match {
      case fullSkin: FullSceneSkinFX =>
        val fullFX = fullSkin.fullSceneFX
        logger.debug(s"fullScreenFX mode")
        _mainGroup.visible.value = false
        _cachedPane.effect = null
        fullFX.unbind()
        // do NOT want to call add here add(sc3D)
        addTickableOnly(fullFX)
        fullFX.fullWidth <== fx.width
        if (fullSkin.topPaneOn && fullFX.minusHeader) {
          logger.debug("not using full height") // not working for firegame...
          fullFX.fullHeight <== fx.height - _timePane.height
        } else
          fullFX.fullHeight <== fx.height
        for (n <- fullFX.fullSceneNode) {
          if (!fx.children.contains(n)) {
            fx.children.insert(0, n)
            StackPane.setAlignment(n, Pos.BottomCenter)
          }
        }
      case default =>
        logger.debug("regular skin")
        _mainGroup.visible.value = true
        _cachedPane.effect = _cachedPaneReflection
        buildNetwork(doNetBuilt = false)
    }
    // order matters.  add ctls AFTER FullSceneSkin so on top
    for (ctrl <- model.controls)
      control(ctrl)

    // assumption here that a game will provide own controls
    if (_skin.useElementDpad && !system.game) {
      control(_elemDPad.dPad)
      addTickableOnly(_elemDPad)
    }
    selectY(null) // do after gameControls

    // do zoom AFTER skin.layout
    zoom.value = _skin.zoom
    _skin.systemBuilt()
    logger.debug(s"timePane viz = " + _timePane.visible.value)
    // always last
    publish(SkinBuiltEvent(_skin))
  }
  override def removeT(t: T): Unit = {
    super.removeT(t)
    // not using for to save iter alloc
    if (t.node.isDefined) {
      val fxNode = t.node.get
      val delAnimOpt = t.as[TickableFX].map(_.delAnim).getOrElseP(None)
      if (!_doingReset && delAnimOpt.isDefined) {
        val anim = delAnimOpt.get
        _animations(anim) = fxNode
        val userFinished = anim.onFinished.getValue
        anim.onFinished = _ => {
          if (userFinished != null) userFinished.handle(null)
          animEnd(anim, del = true)
        }
        anim.play
      } else
        _elemPane.children.remove(fxNode)
    }
    _skin.clearCanvas(_mainCanvas)
  }
  override def reset(fullReset: Boolean = true): Unit = {
    logger.debug(s"sysFX reset(fullReset = $fullReset)")

    selectedElement.value = null
    _doingReset = true
    for (ot <- _stopAnimation) ot.stop()
    _stopAnimation = None
    for (an <- _scoreAnimation) an.stop()
    _scoreAnimation = None
    _scoreLabelFlash.stop()
    _scoreLabel.opacity = 0.3
    _defaultClockFX.clockNextLevel.stop()
    Sound.stopAll()
    stopEditAnimations()
    _skin.clearCanvas(_mainCanvas)
    _clock.reset()
    super.reset(fullReset)
    _clock.maxTime.value = trial.ticks

    for (c <- gameControls)
      c.reset()
    selectY(null) // do after gameControls

    _timeChart.reset()
    score.value = 0
    winScore.value = trial.winScore
    showScore.value = system.game
    if (_matrixScroll.visible.value) _eqFX.draw(model)
    startTracks()
    fx.requestFocus()
    _doingReset = false
  }
  def resize(w: Double, h: Double): Unit = {
    AnchorPane.setRightAnchor(_defaultClockFX.clockPane, 12.0 * h / 720.0)
    val scale = if (h >= 720) h / 720.0 else 1.0
    _timePane.prefHeight.value = HEADER_H * scale
    _menuPane.prefHeight.value = HEADER_H * scale
    _defaultClockFX.clockPane.scaleX = scale
    _defaultClockFX.clockPane.scaleY = scale
    _nameText.scaleY = scale
    _nameText.scaleX = scale
    _scoreText.scaleY = scale
    _scoreText.scaleX = scale
    _scoreLabel.scaleY = scale
    _scoreLabel.scaleX = scale
    AnchorPane.setLeftAnchor(_scorePane, 60.0 * scale)
    // zoomReset()
  }
  final override def setFPS(fps: Int): Unit = {
    if (_fpsText.isVisible) {
      val text = "FPS: " + String.valueOf(fps) // don't use expensive format here...
      logger.trace(text)
      _fpsText.setText(text)
    }
  }
  override def skin_=(f: SkinType): Unit = {
    for (fullFX <- _skin.as[FullSceneSkinFX].map(_.fullSceneFX)) {
      logger.debug(s"clean fullScreen")
      for (n <- fullFX.fullSceneNode) {
        fx.children.remove(n)
        StackPane.clearConstraints(n)
      }
      fullFX.unbind()
    }
    super.skin_=(f)
  }
  override def updateScore(): Unit = {
    if (trial.state == TrialState.Won) {
      val msg: Option[MessageEvent] =
        if (trial.bonus > 0)
          Some(
            MessageEvent(
              s"Bonus points for ${trial.goals.filter(_.bonusCalc(trial) > 0).map(_.bonusMessage).mkString(",")}"))
        else
          None

      updateScoreHelper(trial.score) {
        if (trial.timeBonus > 0) {
          engine.publish(MessageEvent("Time bonus!"))
          updateScoreHelper(trial.score + trial.timeBonus) {
            for (msg <- msg) {
              engine.publish(msg)
              updateScoreHelper(trial.score + trial.timeBonus + trial.bonus)()
            }
          }
        } else if (msg.isDefined) {
          for (msg <- msg) {
            engine.publish(msg)
            updateScoreHelper(trial.score + trial.bonus)()
          }
        }
      }
    } else
      updateScoreHelper(trial.score)()

  }

  protected def updateScoreHelper(newScore: Int)(finish: => Any = {}): Unit = {
    import scalafx.Includes._
    for (an <- _scoreAnimation) an.stop()
    val oldScore = score.value
    logger.debug(s"score change: $oldScore -> $newScore")
    val step = if (newScore > oldScore) 1 else -1
    val timeline = new Timeline {
      keyFrames = for ((s, i) <- ((oldScore + step) to newScore by step).zipWithIndex)
        yield
          KeyFrame(
            time = (500 * (i + 1) ms),
            name = "frame" + i,
            onFinished = e => {
              score.value = s
              val vol = 1.0
              if (step > 0)
                _scorePosSound.start(volume = vol)
              else
                _scoreNegSound.start(volume = vol)
            }
          )
      onFinished = e => { finish }
    }
    logger.trace("keyframe count = " + timeline.keyFrames.size())
    _scoreAnimation = timeline
    timeline.playFromStart
    _scoreLabelFlash.playFromStart()
  }

  final override def tick(te: Tick): Unit = {
    _skin.clearCanvas(_mainCanvas)
    val n = _tickables.length
    var i = 0
    while (i < n) {
      _tickables(i).tick(te)
      i += 1
    }
    var j = 0
    val n2 = _gameControls.length
    while (j < n2) {
      _gameControls(j).tick()
      j += 1
    }
    if (te.quarter)
      _clock.time.value = te.t
    _dirty = false
  }
  override def trial_=(t: Trial): Unit = {
    if (this.trial != t) {
      super.trial_=(t)
      _timeChart.init(trial)
    }
  }
  def zoomReset(): Unit = {
    zoom.value = 1.0
    _mainGroup.translateX.value = (fx.getWidth - Display.width) / 2.0
    _mainGroup.translateY.value = (fx.getHeight - Display.height) / 2.0
    fx.requestFocus()
  }

  protected def addHelper(t: T): Unit = {
    if (t.node.isDefined) {
      val fx = t.node.get
      _elemPane.children.add(fx)
      for (t <- t.as[TickableFX]) {
        if (t.mouseNode.isDefined) {
          val mfx = t.mouseNode.get
          mfx.handleEvent(MouseEvent.Any) {
            handleMouseEvent(_)
          }
        }
        if (t.addAnim.isDefined) {
          val anim = t.addAnim.get
          _animations(anim) = fx
          val userFinished = anim.onFinished.getValue
          if (userFinished != null) {
            anim.onFinished = e => {
              userFinished.handle(e)
              animEnd(anim, del = false)
            }
          } else {
            anim.onFinished = e => animEnd(anim, del = false)
          }
          anim.play
        }
      }
    }
  }
  protected def addTickableOnly(t: TickableFX): Unit = {
    _tickables += t
    t.init()
  }
  protected def animEnd(anim: Transition, del: Boolean): Unit = {
    for (node <- _animations.get(anim)) {
      _animations -= anim
      if (del) _elemPane.children.remove(node)
    }
  }
  protected def equation(y: Y = null) = {
    if (_matrixScroll.visible.value) {
      _eqFX.draw(model = model, y = Option(y))
      _matrixScroll.layout()
    }
  }
  protected def isTimeChartVisible: Boolean = {
    _skin.chartOn &&
    !_skin.customTimeChart &&
    _skin.customPanel.isEmpty
  }
  protected def handleMouseEvent(me: MouseEvent): Unit = {
    import MouseEvent._
    try {
      val evtType = me.eventType
      // way too many move events.  enter/exit target also triggered a lot on Phase3D...
      if (evtType == MouseMoved || evtType == MouseEnteredTarget || evtType == MouseExitedTarget) {} else {
        //        logger.debug("eventType: " + evtType)
        me.userData match {
          case elem: Element =>
            val usOpt = trial.findRealTime(elem)
            logger.trace(elem + " has usOpt: " + usOpt.isDefined)
            evtType match {
              case MouseClicked =>
                logger.trace("mouse clicked: " + elem)
                selectedElement.value = elem
              case MouseDragged if usOpt.isEmpty && model.allowRT =>
                for {
                  tfx <- find(elem)
                  src <- tfx.findTargetRaw(me.delegate.getX, me.delegate.getY)
                } {
                  logger.trace("mouse user start: " + src.tgt)
                  publish(RealStartEvent(y = src.tgt, elem = elem, src = src))
                }
              case MouseDragged if usOpt.isDefined =>
                val us = usOpt.get
                if (us.src != null)
                  publish(RealSampleEvent(us, us.src.calcAct(me.delegate.getY)))
              case MouseReleased if usOpt.isDefined && (me.clickCount == 2 || model.autoReleaseRT) =>
                val us = usOpt.get
                logger.trace("mouse user end: " + us.tgt)
                publish(RealEndEvent(rt = us))
              case default =>
            }
          case default =>
        }
      }
    } catch {
      case th: Throwable =>
        publish(ErrorEvent("mouse error", th))
    }
  }
  protected def keyHandler(ke: KeyEvent): Unit = {
    import javafx.scene.input.KeyCode._
    import javafx.scene.input.KeyEvent._
    for (ctrl <- gameControls) ctrl.keyHandler(ke)
    if (!ke.consumed) {
      if (ke.delegate.getEventType == KEY_PRESSED) {
        val code = ke.delegate.getCode
        code match {
          case BACK_SPACE =>
            publish(ResetEvent(trial))
          case ENTER =>
            publish(new TogglePlayEvent())
          case E if ke.delegate.isControlDown =>
            publish(new EditorEvent)
          case L if ke.delegate.isControlDown =>
            publish(new LibraryEvent)
          case EQUALS =>
            publish(SpeedEvent(inc = true))
          case MINUS =>
            publish(SpeedEvent(inc = false))
          case NUMPAD8 | DIGIT8 =>
            publish(FlashTitleEvent(repeat = 2))
          case NUMPAD9 | DIGIT9 =>
            logger.debug("capture detected")
            publish(new CaptureEvent)
          case default =>
        }
      }
    }
  }
  protected def selectY(y: Y): Unit = {
    logger.debug("selectedY: " + y)
    _timeChart.select(y)
    equation(y = y)
    _elemDPad.dPad.disabled = y == null
    publish(SelectEvent(y = y))
  }
  protected def stopEditAnimations(): Unit = {
    for ((a, fx) <- _animations) {
      a.stop // does not trigger onFinished...
      animEnd(a, del = true)
    }
    _animations.clear
  }
}
