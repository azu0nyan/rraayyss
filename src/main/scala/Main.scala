import rraayyss.{Game, LoadMapFromImage, Render, RenderParams, WorldMap}
import utils.math.V2
import window.GameWindow

import java.awt.{Color, Graphics2D}
import javax.swing.{JFrame, WindowConstants}



object Main {
  def main(args: Array[String]): Unit = {
//    Drawing.startDrawingThread()

    val window = new GameWindow()
    window.startDrawingThread((1378, 766), true)

//    val g = new Game(map = WorldMap.fromPerlinNoise(V2(32, 32)))
    val g = new Game(map = LoadMapFromImage.load("data/maps/lvl1/", 64, 64), window = window)
//    g.map.explode(new V3(4d, 5d, 0.0d), 1.5d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 0.0d), 0.25d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 1.0d), 0.25d, Color.RED.getRGB)
//
//    g.map.explode(new V3(4d, 4.5d, 0.0d), 0.15d, Color.RED.getRGB)
//    g.map.explode(new V3(4d, 5d, 0.0d), 0.15d, Color.CYAN.getRGB)
//    g.map.explode(new V3(5d, 5d, 1.0d), 0.25d, Color.RED.getRGB)

//    g.map.explode(new V3(5d, 5d, 1.0d), 0.25d, Color.RED.getRGB)
    window.addDrawable(g, Int.MaxValue)

    val r = new Render(g, RenderParams(lt = V2(0, 20), size = V2(1078, 766), minimaplt = V2(1078, 200), minimapSize = V2(300, 300)))
    window.addDrawable(r)
    window.addDrawer(gr => gr.drawString(f"${g.position.x}%.3f ${g.position.y}%.3f", 100, 50))


  }

}


