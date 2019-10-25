package memnets.fx.demo

import memnets.core.ModelBuilder
import memnets.fx.app.AppBaseFX
import memnets.model.having

class KDslLayerFX : AppBaseFX() {
    override fun startUpBuilder(): ModelBuilder {
        return ModelBuilder.create("DSL Layer") {
            it.apply {

                val size = 1024
                val freq = 3.0
                val dampen = -0.05

                val x = Layer(size) having {
                    name = "x"
                    ui.plot having {
                        width = 900.0
                    }
                }

                val dx = Layer(size, dampen) having {
                    name = "dx"
                    ui.skip() // hide in UI
                }
                dx.linkTo(x)   // x = dx
                x.linkTo(dx, -freq * freq)

                Trial(10 * 3600, "Phase Shift") having {
                    setOnReset {
                        val mag = 4.0
                        val phaseShift = 2.0 * Math.PI / x.length()
                        val len = x.length() - 1
                        for (i in 0..len) {
                            x.update(i, mag * Math.cos(i * phaseShift))
                            dx.update(i, mag * -freq * Math.sin(i * phaseShift))
                        }
                    }
                }
            }
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            launch(KDslLayerFX::class.java, *args)
        }
    }
}