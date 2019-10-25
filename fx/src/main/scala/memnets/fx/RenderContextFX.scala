package memnets.fx

object RenderContextFX {
  private[fx] var _gc: GC = _
  private[fx] var _fgc: GC = _
  def gc: GC = _gc
  def fgc: GC = _fgc
}
