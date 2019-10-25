package memnets.model

trait AbstractLayer extends LayerLike with LinkableLayer with Config {
  def isSpiking(y: Yb): Boolean
  def src: Tgt = this
  def ui: LayerUI

  /** tick.t at which last ran gpuSync.  ignore for default Sim */
  def lastGpuSync: Int
  override def toString = s"LayerBase[name = $name, n = $length]"

  def getLastGpuSync: Int = lastGpuSync
  override def getUI: LayerUI = ui
}
