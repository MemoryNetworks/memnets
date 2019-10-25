package memnets.fx.utils

import javafx.beans.property.Property
import javafx.beans.property.adapter._
import memnets.model.Logging
import scalafx.Includes._
import scalafx.beans.property._
import scalafx.scene.control.TableColumn.CellEditEvent
import scalafx.scene.control._
import scalafx.scene.control.cell._
import scalafx.util.converter.{BooleanStringConverter => _, _}

object TableColumnUtils {
  import javafx.{beans => jfxb}
  import jfxb.{property => jfxp}

  import scala.reflect.runtime.universe._

  class NumberStringConverterP(val tgt: jfxp.StringProperty, val src: jfxp.Property[Number])
      extends javafx.util.converter.NumberStringConverter
      with Logging {
    import scala.util.Try
    //tgt.setValue(this.toString(src.value)) // set b4 bind!
    tgt.bindBidirectional(src, this)
    override def fromString(s: java.lang.String): java.lang.Number = {
      //      logger.debug(s"$src -> $tgt  converter from # "+s)
      if (s.endsWith(".") || s.endsWith("-"))
        src.getValue
      else
        Try(java.lang.Double.parseDouble(s)).getOrElse(src.getValue).asInstanceOf[Double] // don't change if fail
    }
  }
  class NumColumn[T](
      text_ : String,
      f: T => Property[Number],
      converter: NumberStringConverter = new NumberStringConverter)
      extends TableColumn[T, Number] {
    text = text_
    cellFactory = (c: TableColumn[T, Number]) => new TextFieldTableCell[T, Number](converter)
    cellValueFactory = x => f(x.value)
    onEditCommit = (evt: CellEditEvent[T, Number]) => {
      f(evt.rowValue).setValue(evt.newValue)
      afterCommit(evt)
    }
    def afterCommit(evt: CellEditEvent[T, Number]): Unit = {}
  }
  class StringColumn[T](text_ : String, val f: T => StringProperty) extends TableColumn[T, String] {
    text = text_
    cellFactory = (c: TableColumn[T, String]) => new TextFieldTableCell[T, String](new DefaultStringConverter)
    cellValueFactory = x => f(x.value).delegate
    onEditCommit = (evt: CellEditEvent[T, String]) => { f(evt.rowValue).value = evt.newValue; afterCommit(evt) }
    def afterCommit(evt: CellEditEvent[T, String]): Unit = {}
  }
  type JB = java.lang.Boolean
  class BooleanColumn[T](text_ : String, val f: T => BooleanProperty) extends TableColumn[T, JB] {
    text = text_
    cellFactory = (c: TableColumn[T, JB]) => new CheckBoxTableCell[T, JB]()
    cellValueFactory = x => f(x.value).delegate
    onEditCommit = (evt: CellEditEvent[T, JB]) => { f(evt.rowValue).value = evt.newValue; afterCommit(evt) }
    def afterCommit(evt: CellEditEvent[T, JB]): Unit = {}
  }
  class ColumnBuilder[T <: AnyRef](table: TableView[T], clear: Boolean = true) {
    if (clear) table.getColumns.clear
    def pB(text: String, editable: Boolean = true, prop: Option[String] = None) = {
      val beanProp = prop.getOrElse(text.toLowerCase)
      val edit = editable // rename so not confused below
      val col = new TableColumn[T, JB] {
        cellFactory = (c: TableColumn[T, JB]) => new CheckBoxTableCell[T, JB]()
        cellValueFactory = { x =>
          if (edit)
            BeanUtils.bool(x.value, beanProp).delegate
          else
            BeanUtils.boolRO(x.value, beanProp).delegate
        }
      }
      col.text = text
      col.editable = editable
      addCol(col)
      col.asInstanceOf[TableColumn[T, JB]]
    }
    def pI(text: String, editable: Boolean = true, prop: Option[String] = None) =
      pX(text, classOf[Int], editable, prop).asInstanceOf[TableColumn[T, Number]]
    def pS(text: String, editable: Boolean = true, prop: Option[String] = None) =
      pX(text, classOf[String], editable, prop)
    def pD(text: String, editable: Boolean = true, prop: Option[String] = None) =
      pX(text, classOf[Double], editable, prop).asInstanceOf[TableColumn[T, Number]]
    def pX[S: TypeTag](text: String, s: Class[S], colEditable: Boolean = true, prop: Option[String] = None) =
      this.prop(text, s, colEditable, prop)
    def prop[S: TypeTag](
        text: String,
        s: Class[S] = classOf[Double],
        colEditable: Boolean = true,
        prop: Option[String] = None): TableColumn[T, S] = {
      // NOTE : do NOT want to lowerCase prop
      val beanProp = prop.getOrElse(text.toLowerCase)
      val col = s match {
        case q if q == classOf[Float] =>
          new TableColumn[T, Number] {
            cellFactory = (c: TableColumn[T, Number]) => new TextFieldTableCell[T, Number](new NumberStringConverter)
            cellValueFactory = { x =>
              if (colEditable)
                JavaBeanFloatPropertyBuilder.create.bean(x.value).name(beanProp).build
              else
                ReadOnlyJavaBeanFloatPropertyBuilder.create.bean(x.value).name(beanProp).build
            }
          }
        case q if q == classOf[Double] =>
          new TableColumn[T, Number] {
            cellFactory = (c: TableColumn[T, Number]) => new TextFieldTableCell[T, Number](new NumberStringConverter)
            cellValueFactory = { x =>
              if (colEditable)
                JavaBeanDoublePropertyBuilder.create.bean(x.value).name(beanProp).build
              else
                ReadOnlyJavaBeanDoublePropertyBuilder.create.bean(x.value).name(beanProp).build
            }
          }
        case q if q == classOf[String] =>
          new TableColumn[T, String] {
            cellFactory = (c: TableColumn[T, String]) => new TextFieldTableCell[T, String](new DefaultStringConverter)
            cellValueFactory = { x =>
              if (colEditable)
                JavaBeanStringPropertyBuilder.create.bean(x.value).name(beanProp).build
              else
                ReadOnlyJavaBeanStringPropertyBuilder.create.bean(x.value).name(beanProp).build
            }
          }
        case q if q == classOf[Int] =>
          new TableColumn[T, Number] {
            cellFactory = (c: TableColumn[T, Number]) => new TextFieldTableCell[T, Number](new NumberStringConverter)
            cellValueFactory = { x =>
              if (colEditable)
                BeanUtils.int(x.value, beanProp).delegate
              else
                ReadOnlyJavaBeanIntegerPropertyBuilder.create.bean(x.value).name(beanProp).build
            }
          }
        case q if q.isEnum =>
          new TableColumn[T, Enum[_]] {
            cellFactory = (c: TableColumn[T, Enum[_]]) => new TextFieldTableCell[T, Enum[_]]()
            cellValueFactory = { x =>
              if (colEditable)
                BeanUtils
                  .obj(x.value, beanProp)
                  .delegate
                  .asInstanceOf[javafx.beans.property.adapter.ReadOnlyJavaBeanObjectProperty[Enum[_]]]
              else
                ReadOnlyJavaBeanObjectPropertyBuilder.create
                  .bean(x.value)
                  .name(beanProp)
                  .build
                  .asInstanceOf[javafx.beans.property.adapter.ReadOnlyJavaBeanObjectProperty[Enum[_]]]
            }
          }
        case _ => ???
      }
      col.text = text
      col.editable = colEditable
      addCol(col)
      col.asInstanceOf[TableColumn[T, S]]
    }
    def d(
        text_ : String,
        f: T => Property[Number],
        editable: Boolean = true,
        converter: NumberStringConverter = new NumberStringConverter()): ColumnBuilder[T] = {
      val c = new NumColumn[T](text_, f, converter)
      c.editable = editable
      addCol(c)
      this
    }
    def s(text_ : String, f: T => StringProperty, editable: Boolean = true): ColumnBuilder[T] = {
      val c = new StringColumn[T](text_, f)
      c.editable = editable
      addCol(c)
      this
    }
    def b(text_ : String, f: T => BooleanProperty, editable: Boolean = true): ColumnBuilder[T] = {
      val c = new BooleanColumn[T](text_, f)
      c.editable = editable
      addCol(c)
      this
    }
    def addCol(col: TableColumn[T, _]): ColumnBuilder[T] = {
      // don't want horiz scroll, so subtract vertical scroll size...
      col.prefWidthProperty.bind(
        table.widthProperty.subtract(19).divide(javafx.beans.binding.Bindings.size(table.getColumns)))
      table.getColumns.add(col)
      this
    }
  }

}
