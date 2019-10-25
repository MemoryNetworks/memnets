package memnets

import memnets.model._

package object ui {
  type SkinType = Skin[_, _ <: AnyRef]
  type SF = SkinFactory[SkinType]
  type YGM = YGradientMap[_ <: AnyRef]
  type GF = YGradientMapFactory[_ <: AnyRef]
  val YCOLMAP = "YCOLMAP"
  implicit def skin2Option(f: SkinType): Option[SkinType] = Option(f)
  object NullSkin extends Skin[Unit, String] {
    type UI = TickableUI[Unit]
    type COL = String
    implicit def nt2Option(nt: UI): Option[UI] = Option(nt)
    name = "NullSkin"
    class NullUI(var element: Element) extends UI {
      def node = Some(element.name)
      def tick(te: Tick): Unit = {}
    }
    override def create(elem: Element): Option[UI] = elem match {
      case y: Y =>
        new NullUI(y)
      case g: YGoal =>
        new NullUI(g)
      case s: Signal =>
        new NullUI(s)
      case default =>
        None
    }
    def createY(y: Y): Option[UI] = None // not used
    def convertToColHelper(colorf: Colorf): String = colorf.toString
    protected def createYGradientMap(model: DynamicSystem): YGradientMap[COL] = new YGradientMap[COL] {
      var hints = GradientHints()
      def apply(i: Int, length: Int) = s"$i of $length"
    }
    protected def heatMap: ColorMap[COL] = {
      new ColorMapBase[COL](max = YRange.scale, numColors = 256)(d => d.toString)
    }
    protected def opacityMap: ColorMap[COL] = heatMap
    protected def grayMap: ColorMap[COL] = heatMap
    protected def invertedGrayMap: ColorMap[COL] = heatMap
  }

}
