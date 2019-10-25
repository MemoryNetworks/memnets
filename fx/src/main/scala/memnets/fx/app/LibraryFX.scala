package memnets.fx.app

import javafx.fxml.FXMLLoader
import javafx.scene.{control => jfxc}
import memnets.core._
import memnets.fx._
import memnets.model._
import memnets.utils._
import scalafx.Includes._
import scalafx.collections._
import scalafx.scene.control._
import scalafx.scene.layout.AnchorPane
import scalafx.scene.text.Text
import scalafx.stage.Stage

class LibraryFX(libraries: Iterable[Library]) extends Logging {
  val notags = "no tags"
  val fx = new AnchorPane(FXMLLoader.load("library.fxml".asURL))
  val sciToggle = new ToggleButton(fx.findById[JToggleButton]("sciToggle"))
  val gameToggle = new ToggleButton(fx.findById[JToggleButton]("gameToggle"))
  val authorCombo = new ComboBox(fx.findById[jfxc.ComboBox[Profile]]("authorCombo"))
  val clearBtn = new Button(fx.findById[JButton]("clearBtn"))
  clearBtn.tooltip = Tooltip("Clear all selections")
  val branchList = new ListView(fx.findById[jfxc.ListView[SciBranch]]("branchList"))
  val tagList = new ListView(fx.findById[jfxc.ListView[String]]("tagList"))
  val builderList = new ListView(fx.findById[jfxc.ListView[BldType]]("builderList"))
  val builderTagsList = new ListView(fx.findById[jfxc.ListView[String]]("builderTagsList"))

  val selectedText = new Text(fx.findTextById("selectedLabel"))
  val authorText = new Text(fx.findTextById("authorLabel"))
  val descTextArea = new TextArea(fx.findById[JTextArea]("descTextArea"))

  val builderTags = ObservableBuffer(notags)
  val branches = ObservableBuffer(SciBranch.values.toList)
  val builders = ObservableBuffer[BldType]()
  for (lib <- libraries)
    builders ++= lib.builders
  val builderObs = builders.sorted(Ordering.by[BldType, String](_.name)).filtered(builderFilter(_))
  val tags = ObservableBuffer[String]()

  authorText.text = ""
  authorCombo.items = ObservableBuffer(Profile.profiles.toSeq)
  authorCombo.selectionModel().select(MemNetsProfile)
  // todo : author doesn't filter yet, so hide
  authorCombo.visible.value = false

  selectedText.text = "PLEASE SELECT MODEL"
  builderTagsList.items = builderTags
  builderTagsList.selectionModel.value = null // disable selection

  branchList.items = branches
  branchList.selectionModel().selectionMode = SelectionMode.Multiple
  // init b4 do builders
  sciToggle.selected.value = true
  gameToggle.selected.value = true
  builderList.cellFactory = { p =>
    {
      val cell = new ListCell[BldType]
      cell.item.onChange { (_, _, bld) =>
        if (bld != null)
          cell.text.value = bld.name + (if (bld.modelType == ModelType.Game) "*" else "")
        else
          cell.text.value = ""
      }
      cell
    }
  }
  builderList.delegate.setItems(builderObs)
  tagList.selectionModel().selectionMode = SelectionMode.Multiple
  tagList.delegate.setItems(tags)

  // listeners
  branchList.selectionModel().selectedItemProperty ==> { branch =>
    logger.debug("branch sel = " + branch)
    tagList.selectionModel().clearSelection()
    refresh()
  }
  tagList.selectionModel().selectedItemProperty ==> { tag =>
    logger.debug("tags sel = " + tag)
    if (!tagList.selectionModel().getSelectedItems.isEmpty) {
      // no fullElem fresh, just filter builders
      builderObs.setPredicate(builderFilterWithTags(_))
    }
  }
  builderList.selectionModel().selectedItemProperty ==> { b =>
    logger.debug("builder sel = " + b)
    builderTags.clear
    if (null != b) {
      if (b.tags.isEmpty)
        builderTags += notags
      else
        builderTags ++= b.tags
      selectedText.text = b.name.toUpperCase
      authorText.text = b.author.name

      val desc = b.topic.map(_.prettyPrint(b.description)).getOrElse(b.description)
      descTextArea.text = desc ? "No description available."
    } else {
      builderTags += ""
      selectedText.text = "PLEASE SELECT MODEL"
      authorText.text = ""
      descTextArea.text = null
    }
  }
  sciToggle.selected ==> { _ =>
    branchList.selectionModel().clearSelection()
    tagList.selectionModel().clearSelection()
    refresh()
  }
  gameToggle.selected ==> { _ =>
    branchList.selectionModel().clearSelection()
    tagList.selectionModel().clearSelection()
    refresh()
  }
  clearBtn.onAction = { a =>
    authorCombo.selectionModel().clearSelection()
    branchList.selectionModel.value.clearSelection()
  }

  refreshTags
  def init(builder: BldType): Unit = {
    sciToggle.selected = true
    gameToggle.selected = true
    branchList.selectionModel().clearSelection()
    builderList.selectionModel().clearSelection()

    if (builder != null && builderList.getItems.contains(builder)) {
      try {
        branchList.selectionModel().select(builder.branch)
        // don't worry about tags
        builderList.selectionModel().select(builder)
      } catch {
        case th: Throwable =>
          logger.error("could not initialize: " + builder.name, th)
      }
    }
  }
  def selected = builderList.selectionModel.value.selectedItemProperty()

  protected def builderFilterWithTags(b: BldType): Boolean = {
    if (builderFilter(b)) {
      val sel = tagList.selectionModel.value.getSelectedItems
      b.tags.exists(tag => sel.contains(tag))
    } else
      false
  }
  protected def builderFilter(b: BldType): Boolean = {
    if (b.modelType == ModelType.Sci && !sciToggle.selected.value)
      false
    else if (b.modelType == ModelType.Game && !gameToggle.selected.value)
      false
    else if (branchList.selectionModel.value.getSelectedItems.isEmpty)
      true
    else
      branchList.selectionModel.value.getSelectedItems.contains(b.branch)
  }
  protected def refresh(): Unit = {
    builderObs.setPredicate(builderFilter)
    refreshTags()
  }
  protected def refreshTags(): Unit = {
    import collection.JavaConverters._
    val allTags = collection.mutable.SortedSet[String]()
    for {
      b <- builderObs.asScala
      tag <- b.tags
    } allTags += tag

    tags.clear()
    tags ++= allTags
  }
}

case class LibraryResult(builder: ModelBuilder)

class LibraryDialogFX(stage: Stage, libraries: scala.collection.Seq[Library], initBuilder: Option[BldType] = None)
    extends Dialog[LibraryResult] {
  initOwner(stage)
  title = "MemNets Library"
  headerText = null // important, o.w. spacing issues..
  dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
  val okButton = dialogPane().lookupButton(ButtonType.OK)
  okButton.disable = true
  // tired of trying to find right css selector for this, so darkButtons() it is
  dialogPane().darkButtons()
  dialogPane().stylesheets.add("library.css".asURL.toExternalForm)
  val libFX = new LibraryFX(libraries)
  okButton.disable <== libFX.selected === null
  dialogPane().content = libFX.fx

  resultConverter = b => {
    if (b == ButtonType.OK)
      LibraryResult(libFX.selected.getValue)
    else
      null
  }
//  onShown = e => {
//    chooser.branchList.requestFocus()
//  }
}
