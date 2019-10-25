package memnets.fx.app

import java.io.File

import javafx.fxml.FXMLLoader
import memnets.core.BuiltModel
import memnets.fx._
import memnets.fx.utils._
import memnets.model._
import memnets.utils._
import org.fxmisc.flowless.VirtualizedScrollPane
import scalafx.Includes._
import scalafx.beans.property._
import scalafx.scene.input._
import scalafx.stage.Stage
import scalafx.util.Duration

trait EditorDef {
  def title: String
  def fileDescription: String
  def fileTypes: Iterable[String]
  def codeStyles: CodeAreaStyles
}

/** subclass for testing w/o FX app */
trait EditorOwner {
  def model: BuiltModel
  def open(
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File]

  def save(
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File]

  def fadeDuration(): Duration = (500 ms)
  def hideEditor(fade: Duration = fadeDuration()): Unit = {}
  def openUrl(url: String): Unit
  def showMsg(txt: String): Unit
}

class CodeEditorFX(owner: EditorOwner, editDef: EditorDef) extends Logging {
  protected val _saveFileProp = ObjectProperty[Option[File]](None)
  val editorOpacity = DoubleProperty(1.0)

  private val editLoader = new FXMLLoader("code-editor.fxml".asURL)
  val fx: JBorderPane = editLoader.load()

  val codeArea = new CodeAreaP(editDef.codeStyles)
  val scroll = new VirtualizedScrollPane(codeArea)
  scroll.setId("codeScroll")
  fx.setCenter(scroll)

  val title = StringProperty(editDef.title)
  // editor actions
  val newAction = ActionP("New", '\uf15b') {
    text = ""
    _saveFileProp.value = None
  }
  val openAction = ActionP("Open", '\uf07c') {
    owner.open(
      editDef.fileTypes,
      editDef.fileDescription,
      initFile = _saveFileProp.value
    ) match {
      case Some(f) if f.isFile =>
        try {
          for (inScript <- f.read()) {
            text = inScript
            _saveFileProp.value = Some(f)
          }
        } catch {
          case th: Throwable =>
            _saveFileProp.value = None
            val msg = s"couldn't read file: $f"
            logger.error(msg, th)
            owner.showMsg(msg)
        }
      case Some(f) =>
        // bad file
        _saveFileProp.value = None
      case None =>
      // user cancelled, do nothing
    }
  }
  val saveAction = ActionP("Save", '\uf0c7') {
    if (_saveFileProp.value.isDefined)
      doSave(_saveFileProp.value)
    else
      saveAsAction.forceFire(true)
  }
  val saveAsAction = ActionP("Save As...") {
    val file = owner.save(editDef.fileTypes, editDef.fileDescription, initFile = _saveFileProp.value)
    doSave(file)
  }
  val closeEditorAction = ActionP("Close Editor", '\uf00d') {
    owner.hideEditor()
  }

  val cutAction = ActionP("Cut", '\uf0c4') {
    codeArea.cut()
  }
  val copyAction = ActionP("Copy", '\uf0c5') {
    codeArea.copy()
  }
  val pasteAction = ActionP("Paste", '\uf0ea') {
    codeArea.paste()
  }
  val undoAction = ActionP("Undo", '\uf0e2') {
    codeArea.undo()
  }
  val redoAction = ActionP("Redo", '\uf01e') {
    codeArea.redo()
  }
  val selectAllAction = ActionP("Select All") {
    codeArea.selectAll()
  }
  val selectNoneAction = ActionP("Select None") {
    codeArea.deselect()
  }
  val deleteAction = ActionP("Delete") {
    val selection = codeArea.getSelection
    codeArea.deleteText(selection.getStart, selection.getEnd)
  }
  val wordWrapAction = ActionP.toggle("Word Wrap", initSelect = true) { sel =>
    codeArea.wrapTextProperty.set(sel.isSelected)
  }
  val transparentAction = ActionP.toggle("Transparent", initSelect = false) { sel =>
    editorOpacity.value = if (sel.isSelected) 0.85 else 1.0
  }
  val projectAction = ActionP("Project") {
    owner.openUrl("https://github.com/MemoryNetworks/memnets")
  }
  val helpAction = ActionP("Help", '\uf128') {
    owner.openUrl("https://github.com/MemoryNetworks/memnets/wiki")
  }
  val licenseAction = ActionP("License") {
    owner.openUrl("https://github.com/MemoryNetworks/memnets/blob/master/LICENSE")
  }

  newAction.setAccelerator(KeyCombination("CTRL+N"))
  openAction.setAccelerator(KeyCombination("CTRL+O"))
  saveAction.setAccelerator(KeyCombination("CTRL+S"))
  saveAsAction.setAccelerator(KeyCombination("CTRL+SHIFT+S"))
  cutAction.setAccelerator(KeyCombination("CTRL+X"))
  copyAction.setAccelerator(KeyCombination("CTRL+C"))
  pasteAction.setAccelerator(KeyCombination("CTRL+V"))
  undoAction.setAccelerator(KeyCombination("CTRL+Z"))
  redoAction.setAccelerator(KeyCombination("CTRL+Y"))
  selectAllAction.setAccelerator(KeyCombination("CTRL+A"))
  selectNoneAction.setAccelerator(KeyCombination("CTRL+SHIFT+A"))
  deleteAction.setAccelerator(new KeyCodeCombination(KeyCode.Delete))

  newAction.disabledProperty <== codeArea.hasText.not()
  saveAction.disabledProperty <== codeArea.hasText.not()
  saveAsAction.disabledProperty <== codeArea.hasText.not()

  cutAction.disabledProperty <== codeArea.selectionEmpty
  copyAction.disabledProperty <== codeArea.selectionEmpty
  deleteAction.disabledProperty <== codeArea.selectionEmpty
  selectNoneAction.disabledProperty <== codeArea.selectionEmpty
  // paste can come from outside app and don't know how to tell as of now...
  //  pasteAction.disabledProperty <== _hasPaste.not()
  undoAction.disabledProperty.bind(codeArea.undoAvailableProperty().map(x => !x))
  redoAction.disabledProperty.bind(codeArea.redoAvailableProperty().map(x => !x))

  def text = codeArea.getText()
  def text_=(code: String): Unit = {
    codeArea.setCode(code)
  }
  def bindActions(): Unit = {
    ActionP.lookupActions(editLoader)
  }
  protected def doSave(file: Option[File]): Unit = {
    file match {
      case Some(f) if f.isFile =>
        try {
          val inScript = text
          f.printTo { op =>
            op.print(inScript)
          }
          owner.showMsg(s"saved to file: ${f.getAbsoluteFile}")
          _saveFileProp.value = file
        } catch {
          case th: Throwable =>
            _saveFileProp.value = None
            val msg = s"couldn't save to file: $f"
            logger.error(msg, th)
            owner.showMsg(msg)
        }
      case None =>
    }
  }
  _saveFileProp.onChange { (_, _, fileOpt) =>
    title.value = fileOpt match {
      case Some(file) =>
        editDef.title + ": " + file
      case None =>
        editDef.title
    }
  }
}

/** default impl needs stage */
trait EditorOwnerBase extends EditorOwner {
  def open(
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File] = memnets.fx.open(stage, ext, desc, title, initFile)

  def save(
      ext: Iterable[String],
      desc: String = "",
      title: String = "",
      initFile: Option[File] = None
  ): Option[File] = memnets.fx.save(stage, ext, desc, title, initFile)

  def showMsg(txt: String): Unit
  def stage: Stage
  def openUrl(url: String): Unit
}
