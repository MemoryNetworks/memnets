package memnets.ml

import memnets.model._

/**
 * NaiveBayes algorithm
 * uses m-estimates
 * categoryA is positive, categoryB is negative
 */
class NaiveBayes(val features: Features) extends Learner with Logging {
  val name = "Naive Bayes"
  val probs: Array[Prob] = features.map {
    case df: DiscFeature =>
      if (!df.isOutput)
        DiscProb(df)
      else
        OutputProb(df)
    case cf: ContFeature => ContProb(cf)
  }
  def train(data: Data): Unit = {
    val output = data.output.get
    val posCount = data.count(output.isPos)
    val negCount = data.length - posCount
    for (p <- probs) {
      p.train(data, posCount, negCount)
      // logger.trace(""+p)
    }
  }

  /**
   * using Shavlik "+ vs - odds" classification formulation
   * based on log values
   */
  def classify(pt: Row): Boolean = {
    import Math._
    var positive = 0.0
    var negative = 0.0
    for (p <- probs) {
      // ADD since using logs
      positive += log(p.pos(pt))
      negative += log(p.neg(pt))
    }
    // take DIFF since logs
    (positive - negative) >= 0.0
  }
}
object NaiveBayes {
  val MITCHELL_LAPLACE = 30.0
}
trait Prob {
  def feature: Feature
  def pos(pt: Row): Double
  def neg(pt: Row): Double
  def train(data: Data, posCount: Int, negCount: Int): Unit
}
case class DiscProb(feature: DiscFeature) extends Prob {
  import NaiveBayes._
  val size = feature.values.length // don't use dim b/c output.dim = 0
  val laplaceSmooth = MITCHELL_LAPLACE / size
  // create "+" and "-" for each feature
  // init w/ LaPlace smoothing
  val positives = Array.fill(size) { laplaceSmooth }
  val negatives = Array.fill(size) { laplaceSmooth }
  def pos(pt: Row) = positives(feature.valueIndex(pt))
  def neg(pt: Row) = negatives(feature.valueIndex(pt))
  def train(data: Data, posCount: Int, negCount: Int): Unit = {
    val outF = data.output.get
    for (pt <- data) {
      if (outF.isPos(pt))
        positives(feature.valueIndex(pt)) += 1.0
      else
        negatives(feature.valueIndex(pt)) += 1.0
    }
    // LaPlace smoothing: add # of categories
    val laplacePos = posCount + MITCHELL_LAPLACE
    val laplaceNeg = negCount + MITCHELL_LAPLACE
    var i = 0
    while (i < size) {
      positives(i) /= laplacePos
      negatives(i) /= laplaceNeg
      i += 1
    }
  }
  override def toString = s"${feature}, pos = ${positives.mkString(",")}, neg = ${negatives.mkString(",")}"
}

/**
 * calcs the prior dist.  does NOT look at pt to classify!
 */
case class OutputProb(feature: DiscFeature) extends Prob {
  import NaiveBayes._
  // init with LaPlace smoothing (binary so 2 features)
  var positive = MITCHELL_LAPLACE / 2.0
  var negative = MITCHELL_LAPLACE / 2.0

  def pos(pt: Row) = positive
  def neg(pt: Row) = negative
  def train(data: Data, posCount: Int, negCount: Int) = {
    val outF = data.output.get
    for (pt <- data) {
      if (outF.isPos(pt))
        positive += 1.0
      else
        negative += 1.0
    }
    val total = posCount + negCount + MITCHELL_LAPLACE
    positive /= total
    negative /= total
  }
}

/**
 * NOTE: could be much more efficient. this is naive but easier to follow implementation
 * using Shavlik method #3 to put same # examples in each bin
 */
case class ContProb(feature: ContFeature, numBins: Int = 5) extends Prob {
  import NaiveBayes._
  val laplaceSmooth = MITCHELL_LAPLACE / 2.0
  // create "+" and "-" for each feature
  // init w/ LaPlace smoothing
  val positives = Array.fill(numBins) { laplaceSmooth }
  val negatives = Array.fill(numBins) { laplaceSmooth }
  val limits = Array.ofDim[Double](numBins) // define boundaries of bins
  /*
   * find bin based on feature value of given data point
   */
  private[memnets] def binIndex(pt: Row): Int = {
    val value = feature(pt)
    // resolves ties by placing value in lower bin
    val bin = limits.indexWhere(value <= _)
    if (bin < 0)
      limits.length - 1
    // special case where many repeat values, don't want to take lower bin
    else if (bin < limits.length - 2 && limits(bin) == limits(bin + 1))
      bin + 1
    else
      bin
  }
  def pos(pt: Row) = positives(binIndex(pt))
  def neg(pt: Row) = negatives(binIndex(pt))
  def train(data: Data, posCount: Int, negCount: Int) = {
    val outF = data.output.get
    data.sort((a: Row, b: Row) => feature(a) < feature(b))
    val binSize = data.size / numBins
    var row: Row = null.asInstanceOf[Row]
    for (i <- 0 until numBins) {
      val binOffset = i * binSize
      for (j <- 0 until binSize) {
        row = data(binOffset + j)
        if (outF.isPos(row))
          positives(i) += 1
        else
          negatives(i) += 1
      }
      // use last point as limit for bin boundary
      limits(i) = feature(row)
    }
    // LaPlace smoothing: add # of categories
    var i = 0
    while (i < numBins) {
      val total = positives(i) + negatives(i) + MITCHELL_LAPLACE
      positives(i) /= total
      negatives(i) /= total
      i += 1
    }
  }
  override def toString = s"${feature}, limits= ${limits.pretty}, pos= ${positives.pretty}, neg= ${negatives.pretty}"
}
