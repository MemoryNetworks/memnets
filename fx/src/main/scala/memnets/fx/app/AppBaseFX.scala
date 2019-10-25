package memnets.fx.app

import javafx.geometry.Side
import javax.script.ScriptException
import memnets.core._
import memnets.fx._
import memnets.fx.fx3d.Scene3DFX
import memnets.fx.games.FreeGamesFX
import memnets.fx.utils.JTaskSupport._
import memnets.fx.utils._
import memnets.ml.{Logging => _, _}
import memnets.model._
import memnets.models._
import memnets.ui._
import memnets.utils._
import org.controlsfx.control._
import org.controlsfx.dialog.ProgressDialog
import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.property._
import scalafx.scene._
import scalafx.scene.control.ButtonType
import scalafx.scene.effect.BoxBlur
import scalafx.scene.image._
import scalafx.scene.input._
import scalafx.stage._
import scalafx.util.Duration

import scala.collection.mutable.ArrayBuffer

case class FullSceneEvent(fullFX: FullSceneFX) extends EngineEvent

object AppBaseFX {
  var startupBuilder: () => Option[BldType] = () => None
}

abstract class AppBaseFX extends AppBaseSFX with AppUI with EditorOwnerBase with ScriptEditorOwner with Logging {
  Thread.setDefaultUncaughtExceptionHandler(uncaughtErrorHandler)

  val fxVersion = System.getProperties().get("javafx.runtime.version").toString
  val fxUsesModules = {
    val parts = fxVersion.split("\\.")
    Integer.parseInt(parts.head) > 9
  }
  logger.debug(s"fxVersion: $fxVersion, fxModules: $fxUsesModules")
  // don't change after startup.  just scale...
  Display.resolution = resolution
  logger.debug(s"resolution= ${Display.resolution}")
  // do after set resolution
  val modelFX = new ModelFX()
  modelFX.init()
  val engine = new Engine(modelUI = modelFX, appUI = this)
  object timer extends javafx.animation.AnimationTimer {
    def handle(l: Long): Unit = engine.tick(l)
  }
  val engineFX = new EngineFXAdapter(engine)
  val sideBarFX = new SideBarFX(modelFX, engineFX)
  val libraries = ArrayBuffer[Library]()

  val rootFX = new HiddenSidesPane() {
    setId("root-sides-pane")
    setContent(modelFX.fx)
    setLeft(sideBarFX.fx)
    setTriggerDistance(0.0)
    setAnimationDuration(200 ms)
  }
  val notificationPane = new NotificationPane(rootFX) {
    private val notifyDelay = new PauseTransition(3.0 s)
    notifyDelay.onFinished = { _ =>
      hide()
    }
    def delayedHide(): Unit = {
      notifyDelay.playFromStart()
    }
    getStylesheets.add("notify.css".asURL.toExternalForm)
    setShowFromTop(false)
    setCloseButtonVisible(true)
    setFocusTraversable(false)
  }
  val editor = new ScriptEditorFX(this)
  val editorNotify = new NotificationPane(editor.fx) {
    getStylesheets.add("code-editor-notify.css".asURL.toExternalForm)
    setShowFromTop(false)
    setCloseButtonVisible(true)
  }
  val editPopOver = new PopOver(editorNotify) {
    opacityProperty <== editor.editorOpacity
    titleProperty <== editor.title
    setHeaderAlwaysVisible(true)
    setAnimated(false)
    setCornerRadius(0) // seems a performance cost for rounded
    setArrowLocation(PopOver.ArrowLocation.TOP_CENTER)
  }
  // had issues here w/ release jar.  "popover.css" might have been the problem
  editPopOver.getRoot.getStylesheets.add("popover_custom.css".asURL.toExternalForm)
  editPopOver.onShown = { evt =>
    editPopOver.setDetached(true) // can't just set this when create
    editor.codeArea.requestFocus()
  }
  val windowBarH = 40.0
  val scene = new Scene(width = resolution.width, height = resolution.height - windowBarH) {
    root = notificationPane
    width ==> { w =>
      logger.debug(s"width = $w")
    }
    height ==> { h =>
      logger.debug(s"height = $h")
      modelFX.resize(delegate.getWidth, delegate.getHeight)
      sideBarFX.resize(delegate.getWidth, delegate.getHeight)
    }
  }

  implicit object JTaskSupportImpl extends JTaskSupport {
    override def multiTaskError(): Unit = {
      process(ErrorEvent("app threading logic issue", new Exception("current task not finished")))
    }
    override protected def startTaskHelper[T](task: javafx.concurrent.Task[T]): Unit = {
      showBusy()

      val oldFailed = task.getOnFailed
      task.onFailed = e => {
        logger.debug("task failed")
        _currentTask.value = None
        if (null == oldFailed)
          process(ErrorEvent("task exception", task.getException))
        else
          oldFailed.handle(e)
      }
      val oldSucceeded = task.getOnSucceeded
      task.onSucceeded = e => {
        logger.debug("onSucceeded")
        _currentTask.value = None
        if (!engine.isError) {
          showReady()
          if (null != oldSucceeded) {
            logger.debug("calling orig onSucceeded")
            oldSucceeded.handle(e)
          }
        }
      }
      task.onCancelled = e => {
        logger.debug("onCancelled")
        _currentTask.value = None
        showReady()
      }

      // todo: a bit expensive to make each time...
      new ProgressDialog(task) {
        initStyle(StageStyle.Undecorated)
        initOwner(stage)
        initModality(Modality.WindowModal)
        setHeaderText(null)
        getDialogPane.getStylesheets.add("progress-dialog.css".asURL.toExternalForm)
        getDialogPane.getChildren.remove(0) // nasty hack to kill horrible info icon
        getDialogPane.getScene.setCursor(Cursor.Wait)
      }.show()
    }
  }
  def taskSupport = JTaskSupportImpl

  // use stream not URL here b/c issue when find in jar
  lazy val appIcon = new Image("/memnet_icon320.png".asStream)
  val fullScreenAction = ActionP.toggle("Full screen", '\uf0b2', false, tooltip = "Toggle full screen") { a =>
    stage.fullScreen = a.isSelected
  }
  val validateAction = ActionP.toggle("Validate", '\uf188', false, "Validate") { a =>
    logger.debug("validate action: " + a.isSelected)
  }
  val refreshSkinAction = ActionP("Refresh skin", '\uf021') {
    TaskP("Refreshing skin...") {
      runOnUI {
        sideBarFX.skinEditor.viewModel match {
          case Some(vm) =>
            sideBarFX.skinEditor.propertyEditorFX.dirtyProperty.value = false
            vm.sync()
            modelFX.skin = vm.rootSkin.getOrElse(vm.skin)
            sideBarFX.syncSceneEditor()
          case None =>
        }
      }
    }.start()
  }
  val libraryAction = ActionP("Library", '\uf02d', "Open models library") {
    showLibrary()
  }
  val editAction = ActionP.toggle("Edit", '\uf044', tooltip = "Open editor") { a =>
    onEditAction(a.isSelected)
  }
  val rebuildAction = ActionP("Rebuild", '\uf1b8') {
    rebuild()
  }

  private val _busyBlur = new BoxBlur(10, 10, 2)
  protected val _autoFocusParams = BooleanProperty(true)
  protected var _errorDialog: Boolean = false
  protected object SkinFactory extends SF {
    def create(): SkinType = {
      val batterySkin = new BatterySkinFX()
      batterySkin.name = "Battery*"
      batterySkin
    }
  }
  protected val _config = ModelConfig()

  stage.icons.add(appIcon)
  stage.title = title
  stage.minWidth = 3 * scene.width.value / 4
  stage.minHeight = 3 * scene.height.value / 4
  stage.scene = scene
  stage.onShown = { _ =>
    onAppShown
  }
  stage.onShowing = { _ =>
    onAppShowing
  }
  stage.onCloseRequest = { _ =>
    onCloseRequest
  }
//  stage.fullScreenExitHint = "Esc"
  stage.fullScreenExitKey = KeyCombination("Esc")
  stage.fullScreen ==> { b =>
    fullScreenAction.selected = b
  }

  FullSceneFX._handler = { fullFX =>
    modelFX.publish(FullSceneEvent(fullFX))
  }
  Scene3DFX._handler = { elem =>
    if (modelFX.system != null)
      modelFX.selectedElement.value = elem
  }
  modelFX.menuSelected <==> sideBarFX.showBtn.selected
  modelFX.menuSelected ==> { sel =>
    if (engine.isError) {
      rootFX.setPinnedSide(null)
      showLibrary()
    } else {
      if (sel)
        rootFX.setPinnedSide(Side.LEFT)
      else {
        rootFX.setPinnedSide(null)
        modelFX.fx.requestFocus()
      }
    }
  }

  editPopOver.onHidden = _ => editAction.selected = false
  editAction.enabled = false

  validateAction.selectedProperty <==> engineFX.useValidator
  refreshSkinAction.disabledProperty <== taskSupport.taskRunning ||
    sideBarFX.skinEditor.propertyEditorFX.dirtyProperty.not() ||
    engineFX.playAction.disabledProperty
  rebuildAction.disabledProperty <== taskSupport.taskRunning || engineFX.playAction.disabledProperty

  // todo: might be overkill
  sideBarFX.tabPane.visible <== engineFX.playAction.disabledProperty.not()

  // NOTE : need ot be careful to change on UI thread
  engineFX.paramsModel.selectedItemProperty ==> { showMsg(_) }
  engineFX.inputModel.selectedItemProperty ==> { showMsg(_) }
  editAction.enabled = true

  // make sure to do lookup after all actions created...
  sideBarFX.bindActions()
  editor.bindActions()

  engineFX.speedMod ==> { value =>
    if (value > 1 && value > modelFX.skin.chartSampling)
      showMsg(
        s"NOTE: FF speed ${value}x > skin.chartSampling = ${modelFX.skin.chartSampling} gives incorrect time labels on chart")
  }
  showBusy()

  def config = _config
  def loadResources(): Unit = {
    libraries += DslExamples
    libraries += StandardLibrary
    libraries += FreeGamesFX
  }
  def process(ee: EngineEvent): Unit = ee match {
    case SignalEvent(sig, on) if !sig.isUser && sig.isActive =>
      if (on)
        showMsg(sig)
      else if (sig.descOff != EMPTY_STRING)
        showMsg(sig.descOff)
    case ResetEvent(tri) =>
      showMsg(s"trial preparing: ${tri.description} ")
    case gio: GameIntroOverEvent =>
      showMsg(s"trial starting: ${modelFX.trial.description} ")
    case MessageEvent(msg) =>
      showMsg(msg)
    case TrialDoneEvent(tri) =>
      showMsg(s"trial done: ${tri.description}")
    case SelectEvent(y) =>
      onSelection(y)
    case SkinBuiltEvent(skin) =>
      sideBarFX.setSkin(skin)
    case BuiltModelEvent(model) =>
      postBuild(model)
    case ProgressEvent(msg, workDone, max) =>
      taskSupport.progress(msg)
      taskSupport.progress(workDone, max)
    //  Thread.sleep(100)
    case FullSceneEvent(fullFX) =>
      val skinsModel = engineFX.skinsModel
      val skin = modelFX.skinTyped match {
        case fullSkin: FullSceneSkinFX =>
          logger.trace("fullSceneFX end event")
          fullSkin.prior
        case default =>
          logger.trace("fullSceneFX start event")
          val fullSkin = FullSceneSkinFX(fullFX, prior = default)
          engine.skinsModel.appendItems(fullSkin)
          fullSkin
      }
      skinsModel.select(skin)
      logger.debug("skins : " + skinsModel.getItems.mkString(","))
    case e: EditorEvent =>
      editAction.selected = true
    case le: LibraryEvent =>
      libraryAction.forceFire(true)
    case ErrorEvent(msg, th) =>
      onError(msg, th)
    case default =>
      processHelper(default)
  }
  override def tick(te: Tick): Unit = { sideBarFX.tick(te) }
  def title = "MemNets Community Edition"

  // begin ScriptEditorOwner section
  def model = modelFX.model
  def setBuilder(b: BldType, resetCfg: Boolean = false): Unit = {
    engine.setBuilder(b, resetCfg)
  }
  override def runOnUI[R](op: => R): Unit = {
    runAndWait(op)
  }
  override def task[T](msg: String)(et: EngineTask[T]): Unit = {
    val task = TaskP(msg) { et.call() }
    task.onSucceeded = { e =>
      et.onCompleted(e.getSource.getValue.asInstanceOf[T])
    }
    task.start()
  }
  def showMsg(desc: Descriptable): Unit = {
    if (desc != null && !engine.builderChanging)
      showMsg(desc.description)
  }
  def showMsg(text: String): Unit = {
    runLaterP {
      if (text != null && !text.isEmpty) {
        if (notificationPane.isShowing)
          notificationPane.setText(text)
        else
          notificationPane.show(text)
        notificationPane.delayedHide()
      }
    }
  }
  def showScriptMsg(msg: String): Unit = {
    editorNotify.show() // for some reason???, works better with call to show() 1st
    editorNotify.show(msg)
  }
  def openUrl(url: String): Unit = {
    getHostServices.showDocument(url)
    showMsg(s"Default browser opening: $url")
  }
  override def preScriptBuild(): Unit = {
    engine.error = false // clear error first
    engine.disabled = true
  }
  override def fadeDuration() = editPopOver.getFadeOutDuration
  override def hideEditor(fade: Duration = fadeDuration()): Unit = { editPopOver.hide(fade) }
  // end ScriptEditorOwner section

  // would rather see app window b4 loading
  protected def onAppShowing(): Unit = { logger.debug("onAppShowing") }
  protected def onAppShown(): Unit = {
    logger.debug("onAppShown")

    loadResources()

    libraryAction.enabled = !libraries.isEmpty
    DataSources.dataSourcesModel.selectFirst() // do b4 listener below!!!
    DataSources.dataSourcesModel.onSelection { gen =>
      rebuild()
    }

    timer.start()

    config.skinFactory = SkinFactory
    setBuilder(startUpBuilder)
    for (splash <- _splash)
      splash.close()
  }
  protected def onCloseRequest(): Unit = {
    engine.cleanUp()
    destroyTracks()
  }
  protected def onEditAction(selected: Boolean): Unit = {
    if (selected) {
      runLaterP {
        engine.playing = false
        sideBarFX.hide()
        editPopOver.show(stage, 200, 80)
      }
    } else
      editPopOver.hide(200 ms)
  }
  protected def onError(msg: String, th: Throwable): Unit = {
    th match {
      case se: ScriptException =>
        logger.debug("script error")
        editor.scriptError(se)
        showReady()
      case ce: CsvException =>
        logger.debug("csv error")
        engine.setError(true)
        showMsg(ce.getMessage)
      case re: RuntimeException =>
        logger.debug("runtime error: ")
        re.getCause match {
          case pe: java.text.ParseException =>
            //   engine.setSystemError(error = true)
            showMsg(s"bad input: ${pe.getMessage} (might be recoverable if corrected)")
          case default =>
            onErrorHelper(msg, th)
        }
      case default =>
        onErrorHelper(msg, th)
    }
  }
  protected def onErrorHelper(msg: String, th: Throwable): Unit = {
    logger.error(msg, th)
    engine.setError(true)
    if (!_errorDialog) {
      _errorDialog = true
      runLaterP {
        /* todo
        for (lastTask <- _currentTask.value) {
          if (lastTask.isRunning) {
            logger.error("error: canceling last task")
            lastTask.cancel
          }
        }
         */
        val dialog = ErrorDialogFX(th, libraries.isEmpty)
        for (button <- dialog.showAndWait()) {
          _errorDialog = false
          button match {
            case ButtonType.OK =>
              engine.setError(false)
              showLibrary()
            case ButtonType.Close =>
              stage.close()
          }
        }
      }
    }
  }
  protected def showLibrary(): Unit = {
    showBusy()
    editPopOver.hide()
    logger.debug("builder: garbage collection")
    System.gc() // reclaim some mem b4 new builder

    modelFX.menuSelected.value = false
    engine.disabled = true
    val dialog = new LibraryDialogFX(stage, libraries)
    if (!engine.isError && engine.model != null)
      dialog.libFX.init(engine.model.builder)

    val result = dialog.showAndWait()
    result match {
      // NOTE: doesn't check if the same
      case Some(cr: LibraryResult) =>
        setBuilder(cr.builder, resetCfg = true)
      case default =>
        logger.debug("cancel")
        if (!engine.isError) {
          engine.disabled = false
          showReady()
        } else
          stage.close()
    }
  }
  protected def onSelection(y: Y): Unit = {}
  protected def postBuild(b: BuiltModel): Unit = {
    logger.debug("postBuild")
    stage.title.value = title + " - " + b.name

    sideBarFX.configEditor.setConfig(config)

    if (_autoFocusParams.value)
      sideBarFX.tabPane.getSelectionModel.select(sideBarFX.paramTab)
    // always reset.  rebuild is only case where clear
    _autoFocusParams.value = true
    sideBarFX.dataTab.disable = !engine.model.usesData

    validateAction.enabled = engine.model.validator != NULL_TICK_LISTENER
    editor.postBuild()

    if (!engine.isError)
      editorNotify.hide()

    if (editPopOver.isShowing)
      editor.codeArea.requestFocus()
  }

  /** here for subclasses */
  protected def processHelper(event: EngineEvent): Unit = {
    logger.trace("unprocessed: " + event)
  }
  protected def rebuild(): Unit = {
    if (engine.model != null) {
      // user might already have tab open
      _autoFocusParams.value = false
      // reclaim space
      logger.debug("issuing garbage collection...")
      System.gc()

      setBuilder(engine.model.builder)
    }
  }

  /** for 1080, use named param res, e.g.,  "java --res=1080" */
  protected def resolution: Resolution = {
    parameters.getNamed.getOrDefault("res", "720") match {
      case "1080" =>
        HD1080
      case "900" =>
        HD900
      case default =>
        HD720
    }
  }
  protected def showBusy(): Unit = {
    logger.debug("showBusy")
    modelFX.fx.effect = _busyBlur
    modelFX.fx.mouseTransparent = true
    stage.getScene.getRoot.setCursor(Cursor.Wait)
  }
  protected def showReady(): Unit = {
    logger.debug("showReady")
    modelFX.fx.effect = null
    modelFX.fx.mouseTransparent = false
    stage.getScene.getRoot.setCursor(Cursor.Default)
    modelFX.fx.requestFocus()
  }
  protected def uncaughtErrorHandler(t: Thread, th: Throwable): Unit = {
    logger.error("uncaught exception on thread = " + t, th)
    process(ErrorEvent("uncaught error", th))
  }
}
