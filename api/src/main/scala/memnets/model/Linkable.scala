package memnets.model

import memnets.model.impl._

trait Linkable {
  def src: Y
  def -->(tgt: Linkable): E = src.system.sparse.addEdge(src, tgt)
  def -->(tgt: AbstractLayer): DivergeTied = src.system.addLink(new DivergeTiedImpl(src, tgt), srcOut = false)
  def -:>(tgt: AbstractLayer): Diverge = src.system.addLink(new DivergeImpl(src, tgt), srcOut = false)
  def -->[T <: Linkable](tgt: Iterable[T]): Iterable[E] = src.system.sparse.addWs(src, tgt.map(_.src))
  def -->(tgt: Linkable*): Iterable[E] = src.system.sparse.addWs(src, tgt.map(_.src))
  def -?>(tgt: Linkable): E = src.system.sparse.find(src, tgt).get

  // Java
  def linkTo(tgt: Linkable): E = -->(tgt)
  def linkTo(tgt: Linkable, w: Double): E = -->(tgt).w = w
  def linkTo(tgt: Linkable, tie: Param): E = -->(tgt).tie_=(tie)(src.system)
  def linkTo(tgt: AbstractLayer): DivergeTied = -->(tgt)
  def linkDivergeTo(tgt: AbstractLayer): Diverge = -:>(tgt)
  def linkTo[T <: Linkable](tgt: Iterable[T]): Iterable[E] = -->(tgt)
  def linkTo(tgt: Linkable*): Iterable[E] = -->(tgt)
  def linkToUnique(tgt: Linkable): E = -?>(tgt)
}
