package memnets.ml

import memnets.utils._

case class CsvData(file: String) extends Logging {
  val features: Features = {
    val meta = file.changeFileExt("meta")
    var features: Features = Array[Feature]()
    try {
      // todo : atuo-generate meta if not found...
      features = Feature.read(meta)
    } catch {
      case th: Throwable =>
        logger.error("could not read csv meta: " + file, th)
        throw th
    }
    features
  }
  logger.debug(s"features : ${features.mkString(",")}")
  def data(): Data = {
    try {
      val data = Data.read(features, file)
      logger.debug(file + " size = " + data.length)
      data
    } catch {
      case th: Throwable =>
        val msg = s"error reading file: $file"
        logger.error(msg, th)
        throw CsvException(message = msg, th)
    }
  }
}

case class CsvException(message: String = "Unknown data error", throwable: Throwable = null)
    extends Exception(message, throwable)
