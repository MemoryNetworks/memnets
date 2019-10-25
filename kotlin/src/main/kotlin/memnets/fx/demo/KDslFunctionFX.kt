package memnets.fx.demo

import memnets.core.ModelBuilder
import memnets.fx.BatterySkinFX
import memnets.fx.BubbleSkinFX
import memnets.fx.app.AppBaseFX
import memnets.model.Activation.Relu
import memnets.model.F
import memnets.model.having
import memnets.ui.EffectType
import memnets.ui.SkinImage


class KDslFunctionFX : AppBaseFX() {
    override fun startUpBuilder(): ModelBuilder {
        return ModelBuilder.create("Pred vs Prey") {
            it.apply {
                val prey = Y("Prey", 2.0) having {
                    tau = 60.0
                    activation = Relu
                }
                val pred = Y("Pred", -1.0) having {
                    tau = 60.0
                    activation = Relu
                    // same as relu, but setOut can take any double -> double
                    //setOut {  d  -> if (d < 0.0) 0.0 else d  }
                }
                prey.F { -prey.act * pred.act } having {
                    name = "-py * pd"
                    inputs = listOf(prey, pred)
                }
                pred.F { prey.act * pred.act } having {
                    name = "py * pd"
                    inputs = listOf(prey, pred)
                }
                Phase(pred, prey) having {
                    scale = 40.0
                }
                Step(pred) having {
                    on = 0
                    scale = 0.1
                }
                Step(prey) having {
                    on = 0
                    scale = 0.1
                }

                skin = BatterySkinFX() having {
                    backImage = SkinImage.TWO
                }
                skin = BubbleSkinFX() having {
                    backImage = SkinImage.THREE
                    zoom = 1.2
                    canvasEffect = EffectType.Blur
                }
            }
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            launch(KDslFunctionFX::class.java, *args)
        }
    }
}
