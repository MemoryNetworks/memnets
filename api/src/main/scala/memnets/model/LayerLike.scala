package memnets.model

import java.util
import java.util.Optional

import breeze.linalg.DenseVector
import memnets.utils.ConfigMap

trait LayerLike extends DenseVectorLike with ConfigMap with Element {
  def id: Int
  def system: DynamicSystem

  /** NOTE : for AbstractLayer, decay is not in here */
  def outLinks: Iterable[LayerLink]
  def prettyPrint(n: Int = 16): String = {
    this.iterator.take(n).map(x => f"$x%.3f").mkString("[", ",", if (n < length) "]" else ",...")
  }
  def ui: LayerLikeUI

  /** can potentially CREATE a Yb ref on every call, so only use sparingly! */
  def y(i: Int): Yb

  def topK(k: Int = 3, min: Double = Double.NegativeInfinity, infl: Influence = NopInfluence): TopK = {

    if (lastTopK.isEmpty || lastTopK.get.t != system.now.t)
      lastTopK = system.now.topK(this, k, min, infl)

    lastTopK.get
  }
  def randK(k: Int = 3): TopK = {
    val tick = system.now
    if (lastTopK.isEmpty || lastTopK.get.t != tick.t) {
      import scala.util.Random
      val rand = for {
        i <- 0 until k
        index = Random.nextInt(length)
        y = this.y(index)
      } yield new TopKEntry(index, y.act, y)
      lastTopK = TopK(this, rand, tick.t)
    }
    lastTopK.get
  }
  def lastTopK: Option[TopK]

  /** only system should use */
  def lastTopK_=(topK: TopK): Unit

  // DenseVectorLike
  def apply(i: Int): Double = system.now(id, i)
  def update(i: Int, v: Double): Unit = system.now.update(id, i, v)
  def toDenseVector(output: DenseVector[Double] = null): DenseVector[Double] = { system.now.toDenseVector(this) }
  def :=(other: DenseVector[Double]): this.type = { system.now.replace(this, other); this }
  def clear(): Unit = system.now.clearLayer(this)
  def random(): this.type = { system.now.random(this); this }

  // Jave
  import memnets.utils.JavaUtils._

  import collection.JavaConverters._
  def getId: Int = id
  def getSystem: DynamicSystem = system
  def getLastTopK: Optional[TopK] = lastTopK
  def getOutLinks: util.Collection[LayerLink] = outLinks.asJavaCollection
}
