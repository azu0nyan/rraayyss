package rraayyss

import utils.math.V2

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Path
import javax.imageio.ImageIO

object LoadMapFromImage {
  def load(path: String, sizeX: Int, sizeY: Int): WorldMap = {
    val overlaySize = 32
    val files = new File(path).listFiles().sortBy(_.getName)

    val cells: Array[Array[Cell]] = Array.fill[Cell](sizeX, sizeY)(Cell())

    for {f <- files
         image = ImageIO.read(f)
         texture <- TextureLibrary.getTexture(f.getName)
         x <- 0 until sizeX
         y <- 0 until sizeY
         rgb = image.getRGB(x, y)
         c = new Color(rgb, true) if c.getAlpha > 0
         }
      if (f.getName.startsWith("wall")) {
        cells(x)(y) = cells(x)(y).copy(
          wallX = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))),
          wallX1 = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))),
          wallY = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))),
          wallY1 = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))),
        )
      } else if (f.getName.startsWith("floor")) {
        cells(x)(y) = cells(x)(y).copy(floor = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))))
      } else if (f.getName.startsWith("ceil")) {
        cells(x)(y) = cells(x)(y).copy(ceil = Some(TexturedPlane(Col(c.getRed, c.getGreen, c.getBlue), texture, new BufferedImage(overlaySize, overlaySize, BufferedImage.TYPE_INT_ARGB))))
      }

    val b = TextureLibrary.getTexture("billy_pixel_1.png")

    /*for{
      x <- 0 until sizeX
      y <- 0 until sizeY
    } {
      if(!cells(x)(y).hasWall){
        cells(x)(y) = cells(x)(y).copy( sprites = Seq(Sprite(None, V2(x + .5, y + .5), (V2(-1, -1), V2(1, 1)), b.get)))
      }
    }*/

    cells(1)(1) = cells(1)(1).copy( sprites = Seq(Sprite(Some(V2(-1, 0)), V2(1.0, 1 + .5), (V2(-0.5, -1), V2(0.5, 1)), b.get)))

    println(s"Loaded ${cells.flatten.count(_.hasWall)} walls")
    println(s"Loaded ${cells.flatten.count(_.floor.nonEmpty)} floors")
    println(s"Loaded ${cells.flatten.count(_.ceil.nonEmpty)} ceils")

    WorldMap(sizeX, sizeY, cellSize = 1, cell = cells.apply, overlaySize = overlaySize)
  }
}
