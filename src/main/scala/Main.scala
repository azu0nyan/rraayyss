import drawing.Drawing
import rraayyss.{Game, LoadMapFromImage, Render, WorldMap}
import utils.math.planar.V2
import utils.math.space.V3

import java.awt.{Color, Graphics2D}
import javax.swing.{JFrame, WindowConstants}



object Main {
  def main(args: Array[String]): Unit = {
    Drawing.startDrawingThread()
//    val g = new Game(map = WorldMap.fromPerlinNoise(V2(32, 32)))
    val g = new Game(map = LoadMapFromImage.load("data/maps/lvl1/", 64, 64))
//    g.map.explode(new V3(4d, 5d, 0.0d), 1.5d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 0.0d), 0.25d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 1.0d), 0.25d, Color.RED.getRGB)
//
//    g.map.explode(new V3(4d, 4.5d, 0.0d), 0.15d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 0.0d), 0.15d, Color.CYAN.getRGB)
//    g.map.explode(new V3(5d, 5d, 1.0d), 0.25d, Color.RED.getRGB)

//    g.map.explode(new V3(5d, 5d, 1.0d), 0.25d, Color.RED.getRGB)
    Drawing.addDrawable(g, Int.MaxValue)

    val r = new Render(g)
    Drawing.addDrawable(r)
    Drawing.addDrawer(gr => gr.drawString(f"${g.position.x}%.3f ${g.position.y}%.3f", 100, 50))


  }

}


