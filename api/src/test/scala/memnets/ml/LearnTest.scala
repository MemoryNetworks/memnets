package memnets.ml

import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class LearnTest extends JUnitSuite with MustMatchers {
  def testset(name: String) = s"/testsets/examples_$name.csv"
  val name = testset("kjones")
  @Test def parseFeatures = {
    val dataSet = CsvData(name)
  }
  @Test def randomizeTest = {
    val dataSet = CsvData(name)
    val train = dataSet.data()
    train.print(10)
    train.randomize()
    train.print(10)
  }
  def naiveBayes(data: Data): Learner = new NaiveBayes(data.features)
  def kmeans(data: Data): Learner = new KMeans()

  /**
   * NOTE : can't just pass features as some ML algos use data
   */
  @Test def learnRealData: Unit = {
    learnHelper(CsvData(name).data())(naiveBayes(_))
  }
  @Test def learnCont2Features: Unit = {
    learnHelper(DataGens.cont2Features(feats = 32, size = 1000))(naiveBayes(_))
  }
  @Test def learnCont3FeaturesK: Unit = {
    learnHelper(DataGens.cont3Features(feats = 32, size = 1000))(naiveBayes(_), kmeans(_))
  }
  @Test def learn2Features: Unit = {
    learnHelper(DataGens.binary2Features(feats = 32, size = 1000))(naiveBayes(_))
  }
  @Test def learn3Features: Unit = {
    learnHelper(DataGens.binary3Features(feats = 32, size = 1000))(naiveBayes(_))
  }
  def learnHelper(dataSet: Data)(learnerFactories: (Data => Learner)*): Unit = {
    for (f <- learnerFactories)
      crossValidate(dataSet)(f)
  }
}
