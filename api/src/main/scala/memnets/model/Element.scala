package memnets.model

/** the friendly description shown to the user.  */
trait Descriptable {
  def description: String = ""
  def getDescription: String = description
}

/** interface for anything that could have UI */
trait Element extends Descriptable with UIable[ElementUI] with Dsl {
  def name: String
  def name_=(s: String): Unit
  override def description: String = name
  // Java
  def getName = name
  def setName(n: String): Unit = name = n
}
abstract class ElementBase extends Element with ElementUI {
  var name: String = EMPTY_STRING
  var viz = Viz.Default
  protected var _loc = Loc()

  def loc: Loc = _loc
  def loc_=(l: Loc): Unit = { _loc = l }
  def ui: ElementUI = this
}
