package memnets.model

/**
 * NOTE: Yb is an element mainly for realtime signals.
 * typically, you would never put a Yb directly into Skin.create
 */
trait Yb extends Trackable with Element with UIable[YbUI] {

  /** only id in equals.  unique only to layer (same as the index)*/
  def id: Int
  def layerId: Int = layer.id
  def layer: LayerLike
  def system: DynamicSystem
  def act: Double

  /** should only call in a tickListener */
  def update(v: Double): Unit

  // Java
  def getId: Int = id
  def getLayerId: Int = layerId
  def getLayer: LayerLike = layer
  def getAct: Double = act
  def getSystem: DynamicSystem = system
}
