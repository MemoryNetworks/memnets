package memnets.core

import com.typesafe.scalalogging.Logger
import memnets.ml.Data
import memnets.model._
import memnets.ui._
import org.slf4j

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.Buffer

trait Model extends Dsl {
  implicit def system: DynamicSystem
  implicit def lastTrial: Trial
  implicit def phasePlot: PhasePlot

  /** NOTE: giving unique name so doesn't interfere w/ scripts using own "model" variable */
  implicit def thisModel: Model
  implicit def skinFactory: SF
  implicit def workingDir: WorkingDir

  var useDefaultSkins = true
  var autoReleaseRT = false
  var allowRT = true
  def name = system.name
  def name_=(value: String): Unit = system.name = value

  def equation: Buffer[Y]
  def controls: Buffer[GameControl]
  def tracked: Buffer[Trackable]

  def track(linkables: Linkable*): Unit = for (l <- linkables) tracked += l
  def track(linkJList: java.util.Collection[Linkable]): Unit = {
    import collection.JavaConverters._
    track(linkJList.asScala.toSeq: _*)
  }

  def sizeHint(default: Int = 8, min: Int = 2): Int
  def fileHint(default: String): String
  def loadData(): Data
  def usesData: Boolean
  def usesFile: Boolean

  def destroy(): Unit
  def gameLost(): Unit

  /** giving distinct name on purpose. */
  def logr: Logger
  def onEvent: EngineListener
  def onEvent_=(e: EngineListener): Unit
  def skin: Option[SkinType]
  def skin_=(s: SkinType): Unit
  def skins: mutable.Buffer[SkinType]
  def trials: mutable.Buffer[Trial]
  def validator: TickListener
  def validator_=(tl: TickListener): Unit

  // Java
  def getSystem: DynamicSystem = system
  def setOnEvent(e: EngineListener): Unit = onEvent = e
  def getSkins: java.util.List[SkinType] = skins.asJava
  def setSkin(s: SkinType): Unit = skin = s
  def getWorkingDir: WorkingDir = workingDir
  def getLastTrial: Trial = lastTrial
  def getLogger: slf4j.Logger = logr.underlying
}

trait BuiltModel extends Model with Logging {

  /** here solely to know where this came from */
  def builder: ModelBuilder
  def config: ModelConfig
  def buildSim(doHeadTrial: Boolean = true): Sim = {
    val sim = config.simProvider.create(system, config)
    logger.debug(f"ode: ${sim.method} (${system.name})")
    if (doHeadTrial) {
      sim.trial = trials.head
      sim.reset()
    }
    sim
  }
}
