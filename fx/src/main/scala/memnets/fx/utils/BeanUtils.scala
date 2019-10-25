package memnets.fx.utils

import javafx.beans.property.adapter._
import scalafx.beans.property._

object BeanUtils {
  def int[T](bean: T, name: String): IntegerProperty = {
    val jprop = JavaBeanIntegerPropertyBuilder.create.bean(bean).name(name).build()
    new IntegerProperty(jprop)
  }
  def dbl[T](bean: T, name: String): DoubleProperty = {
    val jprop = JavaBeanDoublePropertyBuilder.create.bean(bean).name(name).build()
    new DoubleProperty(jprop)
  }
  def str[T](bean: T, name: String): StringProperty = {
    val jprop = JavaBeanStringPropertyBuilder.create.bean(bean).name(name).build()
    new StringProperty(jprop)
  }
  def obj[T](bean: AnyRef, name: String): ObjectProperty[T] = {
    val jprop: JavaBeanObjectProperty[T] =
      JavaBeanObjectPropertyBuilder.create.bean(bean).name(name).build().asInstanceOf[JavaBeanObjectProperty[T]]
    new ObjectProperty(jprop)
  }
  def bool[T](bean: T, name: String): BooleanProperty = {
    val jprop = JavaBeanBooleanPropertyBuilder.create.bean(bean).name(name).build()
    new BooleanProperty(jprop)
  }
  def boolRO[T](bean: T, name: String): ReadOnlyBooleanProperty = {
    val jprop = ReadOnlyJavaBeanBooleanPropertyBuilder.create.bean(bean).name(name).build()
    new ReadOnlyBooleanProperty(jprop)
  }

}
