package memnets.ml

import breeze.stats.distributions.Uniform

import scala.math.round

object DataGens {
  val uniform = new Uniform(0.0, 1.0)
  def cont2Features(feats: Int, size: Int, prob: Double = 0.95): Data = contFunc(feats, size, prob) { pt =>
    pt(feats / 4) = 1.0
    pt(feats / 2) = 1.0
  }
  def cont3Features(feats: Int, size: Int, prob: Double = 0.95): Data = contFunc(feats, size, prob) { pt =>
    pt(feats / 4) = 1.0
    pt(feats / 2) = 1.0
    pt(feats - 1) = 1.0
  }
  def contFunc(feats: Int, size: Int, prob: Double = 0.95)(f: Array[Double] => Unit): Data = {
    val features = Array.tabulate[Feature](feats + 1) { i =>
      if (i == 0)
        Output(i, "cat", "Y", "N")
      else
        ContFeature(i, "f" + i, 0.0, 1.0)
    }
    val rawData = Array.tabulate(size) { i =>
      val pt = Array.tabulate(feats + 1) { j =>
        uniform.draw()
      }
      val cat = round(pt(0))
      pt(0) = cat
      // Pos(+) example is 0.0, Neg(-) is 1.0
      if (cat < 1.0 && pt(feats / 4) <= prob)
        f(pt)
      pt
    }
    val data = Data(features, rawData)
    data.randomize()
    data
  }
  def binary2Features(feats: Int, size: Int, prob: Double = 0.95) = binaryXFeatures(feats, size, prob) { pt =>
    pt(feats / 4) = 1.0
    pt(feats / 2) = 1.0
  }
  def binary3Features(feats: Int, size: Int, prob: Double = 0.95) = binaryXFeatures(feats, size, prob) { pt =>
    pt(feats / 4) = 1.0
    pt(feats / 2) = 1.0
    pt(feats - 1) = 1.0
  }
  def binaryXFeatures(feats: Int, size: Int, prob: Double = 0.95)(f: Array[Double] => Unit): Data = {
    val features = Array.tabulate[Feature](feats + 1) { i =>
      if (i == 0)
        Output(i, "cat", "Y", "N")
      else
        new BinFeature(i)
    }
    // allocate once.  tabulate copies...
    val rawData = Array.ofDim[Double](size, feats + 1)
    var i = 0
    while (i < rawData.length) {
      val pt = rawData(i)
      var j = 0
      while (j < pt.length) {
        pt(j) = round(uniform.draw)
        j += 1
      }
      val cat = round(uniform.draw)
      pt(0) = cat
      // Pos+ example is 0.0, Neg- is 1.0
      if (cat < 1.0 && uniform.draw <= prob)
        f(pt)
      i += 1
    }

    val data = Data(features, rawData)
    data.randomize()
    data
  }
}
