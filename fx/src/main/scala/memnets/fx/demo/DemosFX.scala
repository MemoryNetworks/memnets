package memnets.fx.demo

import memnets.fx.app._
import memnets.fx.games._
import memnets.fx.games.wta.NeuralOcean
import memnets.models.StandardLibrary._

object LorentzFX extends DemoFX(lorentz)
object RosslerFX extends DemoFX(rossler)

object CoupledMotorFX extends DemoFX(FreeGamesFX.coupledMotor)
object IzhikFX extends DemoFX(izhikevich)
object SpikingFX extends DemoFX(spikingModels)
object FireFlySyncFX extends DemoFX(fireflySync)
object CoupledFX extends DemoFX(coupledN)
object OscPopFX extends DemoFX(oscPop)
object OscPopHeteroFX extends DemoFX(oscPopHetero)

object Wave2DFX extends DemoFX(wave2D)
object HeatFX extends DemoFX(heat)
object HeatGridFX extends DemoFX(heatGrid)
object LifSheetFX extends DemoFX(lifPop)
object PendulumWavesFX extends DemoFX(pendulumWaves)
object DftFX extends DemoFX(dft)
object PdFX extends DemoFX(pdController)

object PageRankFX extends DemoFX(pageRankHier)

object SoftWTAFX extends DemoFX(swta)
object SoftWTALayerFX extends DemoFX(swtaLayer)
object SoftWTADistFX extends DemoFX(swtaDist)
object SpeechFX extends DemoFX(speech)
object FsmFX extends DemoFX(fsm)
object LogoDemo extends DemoFX(logo)
object HybirdFX extends DemoFX(hybrid)

object RezGlideFX extends DemoFX(RezGlide)
object NeuralOceanFX extends DemoFX(NeuralOcean)
