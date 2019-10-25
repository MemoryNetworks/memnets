package memnets.lwjgl.swt

import memnets.core._
import memnets.models._

abstract class DemoGL(val startUpBuilder: BldType) extends AppBaseGL {
  def loadResources(): Unit = {
    sideBar.addLibrary(DslExamples)
    sideBar.addLibrary(StandardLibrary)
  }
}
