package memnets.fx.app

import memnets.core.{Engine, SpeedEvent}
import memnets.fx._
import memnets.fx.utils._
import memnets.ml.DataSources
import memnets.model.Logging
import scalafx.Includes._
import scalafx.beans.binding.Bindings

class EngineFXAdapter(val engine: Engine) extends Logging {
  import memnets.fx.utils.BeanUtils._
  val loop = bool(engine, "loop")
  val useValidator = bool(engine, "useValidator")
  val playing = bool(engine, "playing")
  val speedMod = int(engine, "speedMod")
  val disabled = bool(engine, "disabled")

  val speedText = Bindings.createStringBinding(
    () =>
      speedMod.value match {
        case ff if ff > 1 =>
          s"SPEED: ${ff}x"
        case one if one == 1 =>
          ""
        case slow if slow < 1 =>
          s"SPEED: 1/${-slow}x"
    },
    speedMod
  )
  // need to keep strong ref here for weak listeners...
  private val paramAdapter = new SelectionModelAdapter(engine.paramsModel)
  val paramsModel = paramAdapter.model

  private val trialsAdapter = new SelectionModelAdapter(engine.trialsModel)
  val trialsModel = trialsAdapter.model

  private val skinsAdapter = new SelectionModelAdapter(engine.skinsModel)
  val skinsModel = skinsAdapter.model

  private val inputAdapter = new SelectionModelAdapter(engine.inputModel)
  val inputModel = inputAdapter.model

  private val dataSourcesAdapter = new SelectionModelAdapter(DataSources.dataSourcesModel)
  val dataSourcesModel = dataSourcesAdapter.model

  val playIcon = ActionP.createGlyph('\uf04b')
  val pauseIcon = ActionP.createGlyph('\uf04c')
  // NOTE : not doing cleaner AppActions for now b/c most impls are less than a few lines
  val playAction = ActionP.toggle("Play", '\uf04b', false) { a =>
    }
  playAction.selectedProperty.onChange { (_, _, sel) =>
    logger.debug(s"play: ${sel}")
    // can come from engine.playing, so make sure FX
    runLaterP {
      if (sel) {
        playAction.setLongText("Pause")
        playAction.setGlyph(pauseIcon)
      } else {
        playAction.setLongText("Play")
        playAction.setGlyph(playIcon)
      }
    }
  }
  val resetAction = ActionP("Reset", '\uf04a', "Reset sim") { engine.fireReset() }
  val prevAction = ActionP("Prev", '\uf048', "Previous") {
    if (!trialsModel.isFirstSelected.get && engine.sim.nowTick.t < 120)
      engine.changeTrial(next = false)
    else
      engine.fireReset()
  }
  val speedDecAction = ActionP("Dec", '\uf053', "Slow down sim") { engine.publish(SpeedEvent(inc = false)) }
  val speedIncAction = ActionP("Inc", '\uf054', "Speed up sim") {
    logger.debug("speed inc")
    engine.publish(SpeedEvent(inc = true))
  }
  val nextAction = ActionP("Next", '\uf051') { engine.changeTrial(next = true) }
  val loopAction = ActionP.toggle("Loop", '\uf079', true) { a =>
    logger.debug("loop action : " + a.isSelected)
  }

  // NOTE Action.disable is NOT the right property. it's an implicit conversion to MenuItem.disable
  playAction.selectedProperty <==> playing
  loopAction.selectedProperty <==> loop

  playAction.disabledProperty <== disabled
  loopAction.disabledProperty <== playAction.disabledProperty
  prevAction.disabledProperty <== playAction.disabledProperty
  nextAction.disabledProperty <== trialsModel.isLastSelected || playAction.disabledProperty
  speedIncAction.disabledProperty <== speedMod === engine.speedMods.last || playAction.disabledProperty
  speedDecAction.disabledProperty <== speedMod === engine.speedMods.head || playAction.disabledProperty

}
