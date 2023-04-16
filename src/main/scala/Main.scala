import drawing.Drawing
import rraayyss.{Game, Render, WorldMap}
import utils.math.planar.V2

import java.awt.{Color, Graphics2D}
import javax.swing.{JFrame, WindowConstants}



object Main {
  def main(args: Array[String]): Unit = {
    Drawing.startDrawingThread()
    val g = new Game(map = WorldMap.fromPerlinNoise(V2(16, 16)))
    Drawing.addDrawable(g, Int.MaxValue)

    val r = new Render(g)
    Drawing.addDrawable(r)


  }

}


