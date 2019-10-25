package memnets.model

/** NOTE : should never set loc to null (not checked) */
trait Locatable extends Dsl {
  def loc: Loc
  def loc_=(l: Loc): Unit
  def getLoc: Loc = loc
  def setLoc(l: Loc): Unit = { loc = l }
}
