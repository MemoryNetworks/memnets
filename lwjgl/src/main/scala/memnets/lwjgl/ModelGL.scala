package memnets.lwjgl

import memnets.core.ModelUIBase
import memnets.model.Tick

class ModelGL extends ModelUIBase[Unit, SkinGLBase] {
  def defaultSkin = new BatterySkinGL
  // Swt only uses canvas for this impl
  def addHelper(tfx: T): Unit = {}
  override def tick(te: Tick): Unit = {
    _skin.clearCanvas()
    super.tick(te)
  }
}
