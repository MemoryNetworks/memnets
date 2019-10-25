package memnets.core

import memnets.linalg.NumberType
import memnets.model._

import scala.beans.BeanProperty

object ModelHints {
  implicit def numType2Hints(numberType: NumberType): ModelHints = {
    ModelHints(
      numberType = numberType,
      ode = if (numberType == NumberType.Doubles) OdeMethod.Ode45 else OdeMethod.Ode23
    )
  }

  /** java helpers */
  def create(numberType: NumberType) = ModelHints(numberType = numberType)
  def create(numberType: NumberType, ode: OdeMethod) = ModelHints(numberType = numberType, ode = ode)
  def create(numberType: NumberType, ode: OdeMethod, tau: Double): ModelHints = {
    ModelHints(numberType = numberType, ode = ode, tau = tau)
  }
}
case class ModelHints(
    @BeanProperty var numberType: Option[NumberType] = None,
    @BeanProperty var ode: Option[OdeMethod] = None,
    @BeanProperty var workDir: Option[String] = None,
    @BeanProperty var tau: Option[Double] = None
)

object NeuralHints {
  def apply(workDir: String = "."): ModelHints = {
    ModelHints(
      numberType = NumberType.Floats,
      ode = OdeMethod.Euler,
      workDir = workDir
    )
  }
}
