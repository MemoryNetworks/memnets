package memnets.fx.app

import java.util

import memnets.model._
import org.fxmisc.richtext.Caret.CaretVisibility
import org.fxmisc.richtext._
import org.fxmisc.richtext.model._

trait CodeAreaStyles {
  def computeHighlighting(text: String): StyleSpans[util.Collection[String]]
}

class CodeAreaP(codeStyles: CodeAreaStyles) extends CodeArea with Logging {
  setId("CodeAreaP")
  setParagraphGraphicFactory(LineNumberFactory.get(this))
  // setCache(true)
  richChanges
    .filter(ch => !ch.getInserted.equals(ch.getRemoved)) // XXX
    .subscribe(chg => applyStyles)

  showCaretProperty().setValue(CaretVisibility.ON)

  import javafx.beans.binding._
  val hasText = new BooleanBinding() {
    bind(textProperty)
    protected def computeValue(): Boolean = {
      val text = textProperty.getValue
      text != null && !text.isEmpty
    }
  }
  val selectionEmpty = new BooleanBinding() {
    bind(selectionProperty)
    protected def computeValue(): Boolean = { getSelection.getLength == 0 }
  };
  def moveCaretTo(row: Int, col: Int): Unit = {
    try {
      //area.positionCaret(area.position(row, 0).toOffset)
      if (row >= 0)
        moveTo(position(row, 0).toOffset + col)
    } catch {
      case th: Throwable =>
        logger.warn(s"bad caret posistion ($row, $col)", th)
    }
  }

  def applyStyles: Unit = {
    try {
      setStyleSpans(0, codeStyles.computeHighlighting(getText))
    } catch {
      case th: Throwable =>
        logger.error("bad parse", th)
    }
  }

  def setCode(text: String): Unit = {
    // replaceText(0,0,text)
    replaceText(text)
  }
}
