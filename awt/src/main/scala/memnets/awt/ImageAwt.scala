package memnets.awt

import java.awt.Graphics2D
import java.awt.image.BufferedImage

import memnets.model._

class ImageAwt(grid: GridData, colorMap: ColMap) {
  val bufferedImage = new BufferedImage(grid.w, grid.h, BufferedImage.TYPE_INT_ARGB)
  val g2: Graphics2D = bufferedImage.createGraphics()

  def destroy(): Unit = { g2.dispose() }
  def reset(): Unit = { grid.reset() }
  def tick(te: Tick): Unit = {
    val pal = colorMap
    var r = 0
    var c = 0
    val rlen = grid.rows
    val clen = grid.cols
    while (r < rlen) {
      c = 0
      while (c < clen) {
        val act = grid.act(te, r, c)
        bufferedImage.setRGB(c, r, pal.apply(act).getRGB)
        c += 1
      }
      r += 1
    }
  }
}
