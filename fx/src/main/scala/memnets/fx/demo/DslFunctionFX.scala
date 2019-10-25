package memnets.fx.demo

import memnets.fx.app.DemoFX
import memnets.models.StandardLibrary

object DslFunctionFX
    extends DemoFX(
      // uses models.biology.PredPrey.scala, which shows Y.F usage
      StandardLibrary.preyPrey
    )
