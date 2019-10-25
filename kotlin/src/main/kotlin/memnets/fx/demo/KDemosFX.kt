package memnets.fx.demo

import memnets.core.ModelConfig
import memnets.fx.app.DemoFX
import memnets.models.StandardLibrary


fun main(vararg args: String) {
    // can change to use any defined builder or create your own ModelBuilder
    val bldr = StandardLibrary.lorentz()
    val app = DemoFX(bldr, ModelConfig())
    app.main(args)
}
