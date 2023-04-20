package window

import java.awt.{Color, Font, Graphics2D}

class FpsCounter extends SimpleDrawable{

  var ticks: Seq[Long] = Seq()

  override def drawAndUpdate(g: Graphics2D, dt: Double): Unit = {
    val ct = System.currentTimeMillis()
    ticks = ticks.filter(t => ct - t <= 1000) :+ ct
    g.setColor(Color.RED)
    g.setFont(new Font("", Font.BOLD, 20))
    g.drawString(s"FPS: ${ticks.size}", 1100, 50)
  }
}
