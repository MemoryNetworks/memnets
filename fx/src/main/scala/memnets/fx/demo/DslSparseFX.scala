package memnets.fx.demo

import memnets.fx.app.DemoFX
import memnets.models.DslExamples

object DslSparseFX
    extends DemoFX(
      // NOTE: could put your own ModelBuilder here instead
      DslExamples.dslSparse
    )
