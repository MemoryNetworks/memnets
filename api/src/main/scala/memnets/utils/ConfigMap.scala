package memnets.utils

/**
 * Don't want each sparse to have own scala.Map[String, Any] due to scaling
 * mapKey allows shared backing map if subclasses add prefix/suffix
 * if client specifies type apply[Float]("thres"), it must be valid
 */
trait ConfigMap {
  protected[memnets] def props: collection.mutable.Map[String, Any]
  def mapKey(key: String): String = key
  def keys(prefix: String): Iterator[String] = {
    props.keys.iterator.filter(_.startsWith(prefix)).map(_.replace(prefix, ""))
  }
  def hasFlag(key: String): Boolean = props.contains(mapKey(key))
  def setFlag(key: String, b: Boolean): Unit = if (b) update(key, 1.0f) else remove(key)
  def get[T](key: String): Option[T] = props.get(mapKey(key)).asInstanceOf[Option[T]]
  def apply[T](key: String): T = props(mapKey(key)).asInstanceOf[T]
  def apply(key: String, default: Float): Float = {
    val opt = props.get(mapKey(key))
    if (opt.isDefined) opt.get.asInstanceOf[Float] else default
  }
  def applyD(key: String, default: Double): Double = {
    val opt = props.get(mapKey(key))
    if (opt.isDefined) opt.get.asInstanceOf[Float] else default
  }
  def remove(key: String): Option[Any] = props.remove(mapKey(key))
  def update[T](key: String, v: T): Option[T] = props.put(mapKey(key), v).asInstanceOf[Option[T]]

  def update(key: String, v: Float): Option[Float] = props.put(mapKey(key), v).asInstanceOf[Option[Float]]
  def update(key: String, v: Double): Option[Float] = update(key, v.asInstanceOf[Float])
  def clearConfig() = props.clear()
}
