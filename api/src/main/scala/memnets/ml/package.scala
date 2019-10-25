package memnets

import com.typesafe.scalalogging.StrictLogging

package object ml extends StrictLogging {
  type Logging = com.typesafe.scalalogging.StrictLogging
  type Row = Array[Double]
  type Features = Array[_ <: Feature]
  type Classify = Boolean => Unit
  def crossValidate(dataSet: Data)(create: Data => Learner): Array[Double] = {
    dataSet.randomize()
    val outF = dataSet.output.get
    logger.debug(s"data2 = $dataSet")
    def validate(i: Int, train: Data, test: Data): Double = {
      val learner = create(train)
      learner.train(train)
      var correct = 0.0
      for (pt <- test) {
        val pred = learner.classify(pt)
        val actual = outF.isPos(pt)
        if (pred == actual) correct += 1
      }
      val accuracy = correct / test.size.toDouble
      logger.debug(s"fold = $i, accuracy = $accuracy")
      accuracy
    }
    val results = for ((i, train, test) <- dataSet.folds(10)) yield validate(i, train, test)
    results.toArray
  }
}
