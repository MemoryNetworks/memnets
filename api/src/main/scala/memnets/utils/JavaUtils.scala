package memnets.utils

import java.util.Optional
import java.util.function._

import scala.reflect.ClassTag

/** quick conversions helper.  could integrate w/ JavaConverters better... */
object JavaUtils {

  implicit def opt2optional[T](opt: Option[T]): Optional[T] = asJava(opt)

  def asScala[T](col: java.util.Collection[T]): Iterable[T] = {
    import scala.collection.JavaConverters._
    col.asScala
  }
  def asScala[S, T](func: BiConsumer[S, T]): (S, T) => Unit = { (arg1, arg2) =>
    func.accept(arg1, arg2)
  }
  def asScala[T](func: Consumer[T]): T => Unit = { arg1 =>
    func.accept(arg1)
  }

  def classTag[T](clazz: java.lang.Class[T]): ClassTag[T] = ClassTag(clazz)
  def none[A](): Option[A] = None
  def option[A](a: A): Option[A] = Option(a)
  def some[A](a: A): Option[A] = Some(a)
  def asJava[A](opt: Option[A]): Optional[A] = if (opt.isDefined) Optional.of(opt.get) else Optional.empty()
}
