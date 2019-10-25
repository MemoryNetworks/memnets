package memnets.awt

import java.awt._

import javax.swing._
import memnets.model._

class JButtonSkin extends J2DSkin {
  name = "JButtonSkin"
  val sw = 50
  val sh = 24
  override def createY(ys: Y): Option[JTickable] = new JBattery(ys) {
    val button = new JButton(ys.name, null) {
      setOpaque(true)
      setFont(new Font("Tahoma", Font.PLAIN, 15))
      setSize(sw, sh)
      setLocation(new Point(ys.ui.loc.x.toInt - sw / 2, ys.ui.loc.y.toInt + 40))
      setForeground(Color.BLACK)
    }
    button.addActionListener(ae => {
      println(ys.name + " pressed")
      ys.update(ys.act * 1.1)
    })
    override def node: Option[JComponent] = Option(button)
    override def tick(te: Tick): Unit = {
      super.tick(te)
      val barCol = if (ys.act > 0.0) posCol else negCol
      button.setBackground(barCol)
    }
  }
}
