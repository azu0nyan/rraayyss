import rraayyss.{Game, LoadMapFromImage, Render, RenderParams, WorldMap}
import utils.math.V2
import window.{FpsCounter, GameWindow}

import java.awt.{Color, Graphics2D}
import javax.swing.{JFrame, WindowConstants}



object Main {
  def main(args: Array[String]): Unit = {
    val window = new GameWindow()
    window.startDrawingThread((1378, 766), true)
    window.addDrawable(new FpsCounter)

    val g = new Game(map = LoadMapFromImage.load("data/maps/lvl1/", 64, 64), window = window)
    window.addDrawable(g, Int.MaxValue)


    val cores = Runtime.getRuntime.availableProcessors()
    val threads = if(cores == 1) 1 else math.max(2, cores - 1)

    val r = new Render(g, RenderParams(lt = V2(0, 20), size = V2(1078, 766), minimaplt = V2(1078, 200), minimapSize = V2(300, 300), threads = threads))
    window.addDrawable(r)
    window.addDrawer(gr => gr.drawString(f"${g.position.x}%.3f ${g.position.y}%.3f", 100, 50))


  }
}

