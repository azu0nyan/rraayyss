package rraayyss

import java.awt.Color
import java.io.File
import java.nio.file.Path
import javax.imageio.ImageIO

object LoadMapFromImage {
  def load(path: String): WorldMap = {
    val file = new File(path + "/wall_stone_32.png")
    val walls = ImageIO.read(file)
    val floors = ImageIO.read(new File(path + "/floor_techno_32.png"))

    val cells: Array[Array[Cell]] = Array.fill[Cell](walls.getWidth, walls.getHeight)(EmptyCell)

    for(x <- 0 until floors.getWidth; y <- 0 until floors.getHeight) {
      val rgb = floors.getRGB(x, y)
      val c = new Color(rgb, true)
      if(c.getAlpha > 0){
//        cells(x)(y) = Floor(Col(c.getRed, c.getGreen, c.getBlue), Some("floor_techno_32.png"))
        cells(x)(y) = Floor(Col(c.getRed, c.getGreen, c.getBlue), Some("floor_mosaik_32.png"))
      }
    }

    for (x <- 0 until floors.getWidth; y <- 0 until floors.getHeight) {
      val rgb = walls.getRGB(x, y)
      val c = new Color(rgb, true)
      if (c.getAlpha > 0) {
        cells(x)(y) = Wall(Col(c.getRed, c.getGreen, c.getBlue), Some("wall_stone_32.png"))
      }
    }

    println(cells.flatten.count(_.isInstanceOf[EmptyCell.type ]))
    println(cells.flatten.count(_.isInstanceOf[Floor ]))
    println(cells.flatten.count(_.isInstanceOf[Wall ]))

    WorldMap(xSize =  walls.getWidth, ySize = walls.getHeight, cellSize = 1, cell = cells.apply)
  }
}
