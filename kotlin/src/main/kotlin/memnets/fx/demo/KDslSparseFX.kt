package memnets.fx.demo

import memnets.core.ModelBuilder
import memnets.fx.BatterySkinFX
import memnets.fx.app.AppBaseFX
import memnets.model.*

class KDslSparseFX : AppBaseFX() {
    override fun startUpBuilder(): ModelBuilder {
        return ModelBuilder.create {
            it.apply {

                val y = Y("y")
                val x = Y("x")

                val freq = Param("frequency", 1.0, 0.5)
                freq.desc = "in cycles/sec"
                freq.func = FuncD {
                    val f = Osc.toFreq(it, y.tau)
                    -f * f
                }

                // NOTE: if didn't want param, could use Y("y'", decay = -0.01) above instead
                val dampen = Param("dampen", -1.0, -0.01)
                dampen.func = FuncD { it * DynamicSystem.TAU_DEFAULT() / y.tau }

                x.linkTo(x, dampen)
                x.linkTo(y)
                y.linkTo(x, freq)

                track(listOf(y))

                system.setOnTick {
                    if (it.t % toSec(2) == 0)
                        logger.debug("onTick every 2s")
                }

                Trial(toMin(1), "IC by Step")
                // on = 0 sets initial condition (system forces duration = 0)
                Step(y) having {
                    on = 0
                    scale = 10.0
                }

                Trial(toMin(1), "IC by onReset").setOnReset {
                    y.update(10.0)
                }

                Trial(toMin(1), "sin input")
                Sin(y, toSec(1), Osc.toPeriod(freq.value), Math.PI, 0.5)

                // NOTE : don't need custom layout for small system, but showing how here
                system.setOnLayout {
                    y.ui.loc = Loc().left(100.0)
                    x.ui.loc = y.ui.loc.right(200.0)
                }

                // using specific skin
                skin = BatterySkinFX() having {
                    zoom = 1.6
                    isBackImageOn = false
                    backColor = Colorf.gray(0.1)
                }

                // customizing platform agnostic skin
                skin = Skin {
                    it.isBackImageOn = false
                    it.backColor = Colorf.apply(0.1, 0.1, 0.15, 1.0)
                }

            }
        }.having {
            name = "DSL Sparse API"
            tags = listOf("API", "DSL").asScala()
            description = "oscillating 2nd order system"
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            launch(KDslSparseFX::class.java, *args)
        }
    }
}