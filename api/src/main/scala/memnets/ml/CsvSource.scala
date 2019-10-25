package memnets.ml

object CsvSource {
  def apply(desc: String, feats: Int, file: String): CsvSource = {
    val csv = new CsvSource(feats, file)
    csv.desc = desc
    csv
  }
}
class CsvSource(feats: Int, file: String) extends DataSource {

  /** blow up upon construction if invalid */
  val csvData = CsvData(file)
  protected def dataHelper(feats: Int, n: Int, prob: Double): Data = csvData.data()
}
