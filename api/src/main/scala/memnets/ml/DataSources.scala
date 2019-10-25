package memnets.ml

import memnets.utils.SelectionModel

object DataSources {
  val dataSourcesModel = new SelectionModel[DataSource](
    new DataSource {
      desc = "2 factors (Binary)"
      features = 32
      def dataHelper(feats: Int, n: Int, prob: Double): Data = DataGens.binary2Features(feats, n, prob)
    },
    new DataSource {
      desc = "2 factors (Cont)"
      features = 32
      def dataHelper(feats: Int, n: Int, prob: Double): Data = DataGens.cont2Features(feats, n, prob)
    },
    new DataSource {
      desc = "3 factors (Binary)"
      features = 32
      def dataHelper(feats: Int, n: Int, prob: Double): Data = DataGens.binary3Features(feats, n, prob)
    },
    new DataSource {
      desc = "3 factors (Cont)"
      features = 32
      def dataHelper(feats: Int, n: Int, prob: Double): Data = DataGens.cont3Features(feats, n, prob)
    }
  )
}
