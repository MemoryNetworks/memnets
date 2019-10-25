package memnets.fx.app

import memnets.core.BuiltModel
import memnets.model._
import scalafx.Includes._
import scalafx.scene.text._

import scala.collection.mutable

// NOTE : hackish code.  needs better impl
class EquationFX(val flow: TextFlow) extends Logging {
  assert(flow != null, "null arg: flow")
  import Math._
  import java.lang.String.format
  final def toTxt(sb: mutable.StringBuilder, styleClass: Option[String] = None): Unit = {
    var fxTxt = new Text(sb.toString)
    fxTxt.getStyleClass.add("matrix-row")
    for (sc <- styleClass) fxTxt.getStyleClass.add(sc)
    flow.children += fxTxt
    sb.clear()
  }
  val rowHeadSel = Some("matrix-row-head-sel")
  val rowHead = Some("matrix-row-head")
  val rowSel = Some("matrix-row-sel")
  def clear(): Unit = { flow.children.clear }

  final def digit(sb: StringBuilder, w: Double): Unit = {
    val aw = abs(w)
    if (aw !~ 1.0) {
      if (floor(aw) == aw) sb ++= f" $aw%.0f "
      else if (aw < 1.0) {
        val s = f"$aw%.2f ".replaceAll("0\\.", ".")
        sb ++= s
      } else sb ++= f"$aw%.1f "
    }
  }
  final def pad(sb: StringBuilder, text: String): Unit = sb ++= format("%1$-" + 3 + "s", text)

  // NOTE : caller must sort collection if desired....
  def draw(model: BuiltModel, y: Option[Y] = None): Unit = {
    clear
    if (model != null) {
      val system = model.system
      val vars: Iterable[Y] =
        if (!model.equation.isEmpty) model.equation else system.variables.filter(!_.ui.isSkipped).take(5).toList
      logger.debug(s"nodes = ${vars.mkString(",")}")
      if (vars.isEmpty) {
        return
      }
      val selId = y.map(_.id).getOrElse(-1)
      var minTau = vars.map(_.tau).min // will blow up if empty
      for (n <- vars) {
        val sb = new StringBuilder
        val cTau = n.tau / minTau
        if (cTau !~ 1.0) sb ++= f"${cTau}%.0f " else sb ++= "   "
        // scala padto sucks
        pad(sb, n.description + "'")
        sb ++= " = "
        toTxt(sb, if (y.exists(_ == n)) rowHeadSel else rowHead)
        var head = true
        for (uf <- n.functions) {
          sb ++= s" ${uf.description}"
          toTxt(sb, if (uf.inputs.exists(_.id == selId)) rowSel else None)
          head = false
        }
        for {
          e <- n.ins.take(6)
          w = e.w if w !~ 0.0
        } {
          if (w < 0) sb ++= " - " else if (!head) sb ++= " + "
          head = false
          digit(sb, w)
          pad(sb, system.variables(e.src).toString)
          toTxt(sb, if (e.src == selId) rowSel else None)
        }
        if (n.threshold !~ 0.0f) {
          if (n.threshold < 0.0f) sb ++= f" + ${-n.threshold}%.1f"
          else sb ++= f" - ${n.threshold}%.1f"
        }
        sb ++= "\n"
        toTxt(sb, None)
      }
      // TODO : userfuncs, thresholds
      if (vars.size == 2) {
        val matrix = system.sparse.toMatrix
        if (matrix(0, 0) ~ 0.0 && matrix(0, 1) ~ 1.0) {
          logger.debug("2nd order")
          val sb = new StringBuilder
          sb ++= "   " // align
          sb ++= "2nd Order \n"
          toTxt(sb, Some("matrix-second"))
          val n = vars.head
          sb ++= "   " // no tau for 2nd
          val desc = if (n.description.endsWith("'")) n.description.replace("'", "") else n.description
          pad(sb, desc + "''")
          sb ++= " = "
          val decay = matrix(1, 1)
          if (decay !~ 0.0) {
            sb ++= (if (decay < 0.0) " - " else " + ")
            digit(sb, decay)
            pad(sb, desc + "'")
            toTxt(sb)
          }
          val freq = matrix(1, 0)
          if (freq !~ 0.0) {
            if (freq < 0.0) sb ++= " - " else "   "
            digit(sb, freq)
            pad(sb, desc)
            toTxt(sb)
          }
        }
      }
    }
  }
}
