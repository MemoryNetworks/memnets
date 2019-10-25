package memnets.fx.utils

import com.typesafe.scalalogging.StrictLogging
import javafx.beans.property.ObjectProperty
import javafx.scene.paint.Color
import javafx.scene.{control => jfxc}
import memnets.utils._
import org.controlsfx.control.action.ActionUtils.{ActionTextBehavior, _}
import org.controlsfx.control.action._
import org.controlsfx.glyphfont._
import scalafx.Includes._
import scalafx.scene.control._

import scala.collection.mutable.ListBuffer

object ActionP extends StrictLogging {
  type JNode = javafx.scene.Node
  val TOGGLE = "Toggle"
  GlyphFontRegistry.register("fontawesome", "/fontawesome-webfont.ttf".asStream, 26)

  private val _actions = ListBuffer[ActionP]()
  private val _fAwesome = GlyphFontRegistry.font("fontawesome")

  implicit class ActionExt[T <: ActionP](val a: T) extends AnyVal {
    def isToggle = a.getProperties.containsKey(TOGGLE)
    def toButton = {
      val tb = new Button()
      a <== tb
      tb
    }
    def toToggle = {
      val tb = new ToggleButton()
      a <== tb
      tb
    }
  }
  implicit def toMI(a: ActionP): MenuItem = a.toMenuItem

  def apply(
      title: String,
      unicode: Char = null.asInstanceOf[Char],
      tooltip: String = ""
  )(body: => Any) = {

    new ActionP(title, unicode, tooltip, false)(Action => body)
  }

  def lookupActions(loader: javafx.fxml.FXMLLoader): Unit = {
    val nameSpace = loader.getNamespace
    for (a <- _actions) {
      nameSpace.get(a.actionId) match {
        case btn: javafx.scene.control.ButtonBase =>
          logger.trace(s"found btn : ${a.actionId}")
          a <== btn
        case null    => //logger.debug(s"can't find action : ${a.actionId}")
        case default => ???
      }
      nameSpace.get(a.menuId) match {
        case mi: javafx.scene.control.MenuItem =>
          logger.trace(s"found mi : ${a.menuId}")
          a <== mi
        case null    => // logger.debug(s"can't find menu action : ${a.menuId}")
        case default => ???
      }
    }
  }

  def createGlyph(char: Character): Glyph = {
    if (char != null) {
      val f = _fAwesome.create(char)
      f.color(Color.GRAY)
      f.getStyleClass.add("fawesome")
      f
    } else
      null
  }

  def toggle(
      title: String,
      unicode: Char = null.asInstanceOf[Char],
      initSelect: Boolean = false,
      tooltip: String = ""
  )(body: (Action) => Any) = {

    new ActionP(title, unicode, tooltip, initSelect, true)(body)
  }

  def toContextMenu(actions: ActionP*) = new ContextMenu(actions.map(_.toMenuItem): _*)

  protected def register(ap: ActionP): Unit = { _actions += ap }
}
class ActionP(title: String, unicode: Char, tooltip: String = "", initSelect: Boolean = false, toggle: Boolean = false)(
    body: (Action) => Any = a => {})
    extends Action(title)
    with StrictLogging {
  import ActionP._
  protected val _fxmlTitle = title.toLowerCase.replaceAll("[ .%]+", "")
  protected var _beh: ActionTextBehavior = ActionTextBehavior.HIDE
  protected var _glyph: Glyph = _

  setSelected(initSelect)

  setLongText(if (tooltip != null && !tooltip.isEmpty) tooltip else title)
  if (toggle) {
    getProperties.put(TOGGLE, "true")
    selectedProperty.onChange { (_, _, _) =>
      if (enabled) body(ActionP.this)
    }
  } else {
    setEventHandler(ae => { body(ActionP.this) })
  }
  setGlyph(createGlyph(unicode))
  register(this)

  def actionId = _fxmlTitle + "Action"
  def menuId = _fxmlTitle + "MI"
  def forceFire(sel: Boolean): Unit = {
    if ((isSelected == sel || !toggle) && enabled)
      body(ActionP.this) // force refire
    else
      selected = sel
  }
  def setGlyph(gly: Glyph): Unit = {
    _glyph = gly
    setGraphic(gly)
  }
  def selected: Boolean = selectedProperty.get
  def selected_=(b: Boolean): Unit = { setSelected(b) }
  def <==(btn: jfxc.ButtonBase, beh: Option[ActionTextBehavior] = None): Unit = {
    configureButton(this, btn)
    duplicateGlyphFix(btn.graphicProperty)
    // default is to show.  boo
    beh.getOrElse(_beh) match {
      case ActionTextBehavior.HIDE =>
        // todo : better way?
        btn.textProperty.unbind()
        btn.text = ""
      case default =>
    }
    //btn.cache = true
    btn match {
      case tbj: jfxc.ToggleButton => tbj.selected <==> selectedProperty
      case cbj: jfxc.CheckBox     => cbj.selected <==> selectedProperty
      case _                      =>
    }
  }
  def toMenuItem: MenuItem = {
    val mi = if (this.isToggle) new CheckMenuItem() else new MenuItem()
    this <== mi
    mi
  }
  def <==(btn: jfxc.MenuItem): Unit = {
    ActionUtils.configureMenuItem(this, btn)
    duplicateGlyphFix(btn.graphicProperty)
    btn match {
      case tbj: jfxc.CheckMenuItem => tbj.selected <==> selectedProperty
      case cbj: jfxc.RadioMenuItem => cbj.selected <==> selectedProperty
      case _                       =>
    }
  }
  def enabled = !isDisabled
  def enabled_=(b: Boolean): Unit = { disabledProperty.setValue(!b) }

  // NOTE: for some reason they can't duplicate Glyph's FUNDAMENTAL property (unicode) correctly...
  private def duplicateGlyphFixHelper(node: JNode): Unit = {
    node match {
      case gl: Glyph =>
        if (_glyph != null) {
          val txt = _glyph.getText
          gl.setText(txt)
        }
      case default =>
    }
  }
  private def duplicateGlyphFix(prop: ObjectProperty[JNode]): Unit = {
    duplicateGlyphFixHelper(prop.value)
    //    prop.unbind()
    prop.onChange { (_, _, node) =>
      duplicateGlyphFixHelper(node)
    }
  }
}
