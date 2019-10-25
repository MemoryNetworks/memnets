package memnets.fx.app

import java.util
import java.util.Collections

import memnets.model.Logging
import org.fxmisc.richtext.model._

import scala.collection.mutable

object DslEditorDef extends EditorDef with CodeAreaStyles with Logging {
  import java.util.regex.Pattern
  val f = 4
  private val SCALA_KEYWORDS = Array(
    "val",
    "var",
    "def",
    "object",
    "trait",
    "abstract",
    "Boolean",
    "Byte",
    "case",
    "catch",
    "Char",
    "class",
    "do",
    "Double",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "Float",
    "for",
    "if",
    "extends",
    "import",
    "Int",
    "Long",
    "match",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "Short",
    "super",
    "this",
    "throw",
    "try",
    "true",
    "type",
    "Unit",
    "while",
    "with"
  )
  private val KEYWORD_PATTERN = "\\b(" + SCALA_KEYWORDS.mkString("|") + ")\\b"
  // NOTE: "-->" won't work in DSL
  private val DSL = Array(
    "Y",
    "Loc",
    "Trial",
    "Param",
    "Skin",
    "DenseVector[Float]",
    "DenseVector[Double]",
    "Layer",
    "Input",
    "Dense",
    "SoftMax",
    "LambdaLayer",
    "Grid",
    "YGrid",
    "Sliding",
    "Step",
    "Sin",
    "Cos",
    "Colorf",
    "GradientHints",
    "YGoal",
    "Goal",
    "Conv1D",
    "Element",
    "TickableFX",
    "Osc",
    "OscPop",
    "Trigger",
    "SoftWTA"
  )
  private val DSL_PATTERN = "\\b(" + DSL.mkString("|") + ")\\b"
  // not used
  //  private val OPERATOR = Array("+","-","*","/")
  //  private val OPERATOR_PATTERN = "\\b("+OPERATOR.mkString("|")+")\\b"
  private val CONSTANT_PATTERN = raw"\b[A-Z][A-Z0-9]+\b"
  private val NUMBER_PATTERN = raw"\b[0-9]+[\\.]?[0-9]*[fdL]?\b"
  private val PAREN_PATTERN = "\\(|\\)"
  private val BRACE_PATTERN = "\\{|\\}"
  private val BRACKET_PATTERN = "\\[|\\]"
  private val SEMICOLON_PATTERN = "\\;"
  private val STRING_PATTERN = "\"([^\"\\\\]|\\\\.)*\""
  //  private val COMMENT_PATTERN = "//[^\n]*" + "|" + "/\\*(.|\\R)*?\\*/"
  private val COMMENT_PATTERN = "//[^\n]*"
  private val MULTILINE_COMMENT_PATTERN = "/\\*(.|\\R)*?\\*/"
  private val PATTERN = Pattern.compile(
    "(?<keyword>" + KEYWORD_PATTERN + ")" +
      "|(?<dsl>" + DSL_PATTERN + ")" +
      "|(?<constant>" + CONSTANT_PATTERN + ")" +
      "|(?<paren>" + PAREN_PATTERN + ")" +
      "|(?<brace>" + BRACE_PATTERN + ")" +
      "|(?<bracket>" + BRACKET_PATTERN + ")" +
      "|(?<semicolon>" + SEMICOLON_PATTERN + ")" +
      "|(?<number>" + NUMBER_PATTERN + ")" +
      "|(?<string>" + STRING_PATTERN + ")" +
      "|(?<comment>" + COMMENT_PATTERN + ")" +
      "|(?<multi>" + MULTILINE_COMMENT_PATTERN + ")"
  )
  private val STYLE_CLASSES = Array(
    "keyword",
    "dsl",
    "constant",
    "paren",
    "brace",
    "bracket",
    "semicolon",
    "number",
    "string",
    "comment",
    "multi"
  )
  private val DEFAULT_STYLE = Collections.singleton("text-default")
  private val STYLE_MAP = mutable.AnyRefMap[String, java.util.Collection[String]]()
  for (style <- STYLE_CLASSES) STYLE_MAP(style) = Collections.singleton(style)

  def codeStyles: CodeAreaStyles = this
  def computeHighlighting(text: String): StyleSpans[util.Collection[String]] = {
    val matcher = PATTERN.matcher(text)
    def hasGroup(text: String) = matcher.group(text) != null
    var lastKwEnd = 0
    val spansBuilder = new StyleSpansBuilder[util.Collection[String]]()
    while (matcher.find()) {
      spansBuilder.add(DEFAULT_STYLE, matcher.start() - lastKwEnd)
      val styleCol = STYLE_MAP.find(x => hasGroup(x._1)).get._2
      spansBuilder.add(styleCol, matcher.end() - matcher.start())
      lastKwEnd = matcher.end()
    }
    spansBuilder.add(Collections.emptyList(), text.length() - lastKwEnd)
    spansBuilder.create()
  }
  def title: String = "Script editor"
  def fileDescription: String = "MemNet files"
  def fileTypes: Iterable[String] = List("*.mnet", "*.scala")
}
