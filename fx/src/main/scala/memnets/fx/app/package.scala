package memnets.fx

import memnets.core._

package object app {
  implicit class BuilderAppExt[B <: ModelBuilder](val bld: B) extends AnyVal {

    /** does not work in javaFX 11+ */
    def demo(): Unit = SingletonAppFX.demo(bld)
  }
}
