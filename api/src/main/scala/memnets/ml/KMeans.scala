package memnets.ml

import breeze.linalg.DenseVector
import breeze.stats.distributions.Uniform

import scala.collection.mutable.ArrayBuffer

class KMeans(val k: Int = 3) extends Learner with Logging {
  def name = "K-Means"
  val clusters = Array.ofDim[Cluster](k)
  case class Cluster(val index: Int, val features: Features, val output: DiscFeature) {
    val cFeatures = features.filter(_.isInstanceOf[ContFeature]).map(_.asInstanceOf[ContFeature])
    // must be length of features.  indexes not in cFeatures are ignored
    val center = DenseVector.rand[Double](size = features.length, Uniform(0.0, 1.0))
    val pts = ArrayBuffer[Row]()
    var cat: Boolean = false
    def dist(pt: Row): Double = {
      var sum = 0.0
      val len = cFeatures.length
      var i = 0
      while (i < len) {
        val cf = cFeatures(i)
        val diff = cf.normalized(pt) - cf.raw(center.data)
        sum += (diff * diff)
        i += 1
      }
      Math.sqrt(sum)
    }
    def update(): Double = {
      // compute new center
      val newCenter = DenseVector.zeros[Double](size = features.length)
      if (!pts.isEmpty) {
        val len = cFeatures.length
        var votes = 0
        for (pt <- pts) {
          var i = 0
          while (i < len) {
            val cf = cFeatures(i)
            newCenter(cf.index) += cf.normalized(pt)
            i += 1
          }
          if (output.isPos(pt))
            votes += 1
        }
        cat = votes > pts.size / 2

        val ptsSize: Double = pts.size
        var i = 0
        while (i < len) {
          val j = cFeatures(i).index
          newCenter(j) = newCenter(j) / ptsSize
          i += 1
        }
        pts.clear()
        //      logger.trace(s"cluster[i= $index, ctr= ${center.data.mkString(",")}]")
      }
      val epsilon = dist(newCenter.data)
      center := newCenter
      epsilon
    }
  }
  def train(data: Data): Unit = {
    val output = data.output.get
    for (i <- 0 until clusters.length)
      clusters(i) = Cluster(i, data.features, output)

    val maxIterations = 20
    var iteration = 0
    val epsilon = 1e-3
    var keepGoing = true
    // Execute iterations of Lloyd's algorithm until converged
    while (iteration < maxIterations && keepGoing) {
      // 1. Assign data to closest center
      for (pt <- data)
        nearest(pt).pts += pt

      //  logger.trace("Kmeans: "+clusters(0).pts.size+ " vs "+ clusters(1).pts.size)
      // 2. Centroid update
      var change = false
      for (c <- clusters) {
        if (c.update() > epsilon)
          change = true
      }
      keepGoing = change
      iteration += 1
    }
    //  logger.trace("Stopped at iteration: "+iteration)
  }

  /** NOTE : only using closest cluster */
  def classify(pt: Row): Boolean = nearest(pt).cat
  def nearest(pt: Row): Cluster = clusters.map(x => (x, x.dist(pt))).sortBy(_._2).head._1
}
