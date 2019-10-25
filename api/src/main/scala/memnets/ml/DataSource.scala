package memnets.ml

import memnets.model._

import scala.beans.BeanProperty

abstract class DataSource extends Descriptable with Logging {
  @BeanProperty var desc = "Unknown dataset"
  @BeanProperty var features = 32
  @BeanProperty var probability = 0.95
  @BeanProperty var size = 1000
  def data(): Data = {
    logger.debug("dataset: " + desc)
    val d = dataHelper(features, size, probability)
    d.desc = desc
    d
  }
  override def description: String = desc ? "Unknown dataset"
  protected def dataHelper(feats: Int, n: Int, prob: Double): Data
}
