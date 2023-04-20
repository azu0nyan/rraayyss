package rraayyss

import utils.math.V2
import utils.math.V3

import java.awt.Color
import java.awt.image.BufferedImage
import scala.util.Random


case class Cell(
                 wallX: Option[TexturedPlane] = None,
                 wallX1: Option[TexturedPlane] = None,
                 wallY: Option[TexturedPlane] = None,
                 wallY1: Option[TexturedPlane] = None,
                 floor: Option[TexturedPlane] = None,
                 ceil: Option[TexturedPlane] = None,
               ) {
  def hasWall: Boolean = wallX1.nonEmpty || wallX.nonEmpty || wallY.nonEmpty || wallY1.nonEmpty
}
case class TexturedPlane(colorMultiplier: Col,
                         texture: BufferedImage,
                         overlayTexture: BufferedImage)

case class RayCastResult(
                          hitPos: V2,
                          hitCell: Cell,
                          distance: Double
                        )
object WorldMap {
  //  def fromPerlinNoise(size: V2): WorldMap = {
  //    def cFunc(x: Int)(y: Int): Cell = {
  //      val n = PerlinNoise.noise(x / 3f, y / 3f, 1f)
  //      if (x == 0 || y == 0 || x == (size.x.toInt - 1) || y == (size.y.toInt - 1) || n > 0.5) TexturedPlane(randomColor(x * 11231232 + y * 1111111))
  //      else EmptyCell
  //    }//
  //    WorldMap(xSize = size.xInt, ySize = size.yInt, cell = cFunc)
  //  }
}


case class WorldMap(
                     xSize: Int = 16,
                     ySize: Int = 16,
                     cellSize: Int = 1,
                     cell: Int => Int => Cell,
                     overlaySize: Int = 32,
                   ) {

  var maxX = xSize * cellSize
  var maxY = ySize * cellSize

  def contains(pos: V2): Boolean = pos.x >= 0 && pos.y > 0 && pos.x < maxX && pos.y < maxY

  def indices: Iterator[(Int, Int)] = for (x <- (0 until xSize).iterator; y <- (0 until ySize).iterator) yield (x, y)

  def cellPos(x: Double, y: Double): (Int, Int) = ((x / cellSize).floor.toInt, (y / cellSize).floor.toInt)

  def cellAt(x: Double, y: Double): Option[Cell] = {
    val (xPos, yPos) = cellPos(x, y)
    Option.when(xPos < xSize && xPos >= 0 && yPos < ySize && yPos >= 0)(cell(xPos)(yPos))
  }

  def iterateOverGrid(from: V2, dir: V2, eps: Double = 0.0001d): Iterator[V2] = Seq(from).iterator ++ new Iterator[V2] {
    var t = 0d

    override def hasNext: Boolean = true
    override def next(): V2 = {
      val curPos = from + dir * t
      val (tx, ty) = cellPos(curPos.x, curPos.y)
      val inX = curPos.x - tx * cellSize
      val inY = curPos.y - ty * cellSize

      val dtX =
        if (dir.x > 0) (cellSize - inX) / dir.x
        else if (dir.x < 0) inX / (-dir.x)
        else Double.MaxValue
      val dtY =
        if (dir.y > 0) (cellSize - inY) / dir.y
        else if (dir.y < 0) inY / (-dir.y)
        else Double.MaxValue
      val dt = math.min(dtX, dtY) + eps

      t += dt

      from + dir * t
    }
  }

  def rayCastAllTiles(from: V2, dir: V2, maxLength: Double): Seq[RayCastResult] = {
    iterateOverGrid(from, dir).takeWhile(_.distance(from) <= maxLength).takeWhile(contains)
      .map { case v@V2(x, y) => RayCastResult(v, cellAt(x, y).get, v.distance(from)) }.toSeq
  }

  def rayCastFirstWall(from: V2, dir: V2, maxLength: Double): Option[RayCastResult] = {
    iterateOverGrid(from, dir).takeWhile(_.distance(from) <= maxLength).collect(pos => cellAt(pos.x, pos.y) match
      case Some(c) if c.hasWall => RayCastResult(pos, c, from.distance(pos))
    ).toSeq.headOption
  }


  def explode(at: V3, size: Double, color: Int): Unit = {
    //    import math.BigDecimal.double2bigDecimal
    val overlayPixelSize = 1d / overlaySize.toDouble
    //    for (
    //      xb <- (at.x - size) to (at.x + size) by overlayPixelSize;
    //      yb <- (at.y - size) to (at.y + size) by overlayPixelSize;
    //      zb <- (at.z - size) to (at.z + size) by overlayPixelSize;
    //      c <- cellAt(xb.toDouble, yb.toDouble)
    //    )
    for (
      xb <- ((at.x - size) * overlaySize).toInt to ((at.x + size) * overlaySize).toInt;
      yb <- ((at.y - size) * overlaySize).toInt to ((at.y + size) * overlaySize).toInt;
      zb <- ((at.z - size) * overlaySize).toInt to ((at.z + size) * overlaySize).toInt;
      x = xb / overlaySize.toDouble;
      y = yb / overlaySize.toDouble;
      z = zb / overlaySize.toDouble;
      pos = V3(x, y, z) if pos.distance(at) <= size;
      c <- cellAt(xb / overlaySize.toDouble, yb / overlaySize.toDouble)
    ) {

      val (cposX, cposY): (Int, Int) = cellPos(x, y)
      if (c.hasWall && 0 < z && z <= 1) {
        val zHeight = ((1d - z) * overlaySize).toInt

        if ((x - cposX) <= overlayPixelSize) {
          val xPos = ((y - cposY) * overlaySize).toInt

          val oldColor = c.wallX.get.overlayTexture.getRGB(xPos, zHeight)
          val newColor = Col.combineWithAlpha(oldColor, color)
          c.wallX.get.overlayTexture.setRGB(xPos, zHeight, newColor)
        }
        if (x - cposX >= 1 - overlayPixelSize) {
          val xPos = ((y - cposY) * overlaySize).toInt

          val oldColor = c.wallX.get.overlayTexture.getRGB(xPos, zHeight)
          val newColor = Col.combineWithAlpha(oldColor, color)
          c.wallX1.get.overlayTexture.setRGB(xPos, zHeight, newColor)
        }
        if (y - cposY <= overlayPixelSize) {
          val yPos = ((x - cposX) * overlaySize).toInt

          val oldColor = c.wallX.get.overlayTexture.getRGB(yPos, zHeight)
          val newColor = Col.combineWithAlpha(oldColor, color)
          c.wallY.get.overlayTexture.setRGB(yPos, zHeight, newColor)
        }

        if (y - cposY >= 1 - overlayPixelSize) {
          val yPos = ((x - cposX) * overlaySize).toInt
          val oldColor = c.wallX.get.overlayTexture.getRGB(yPos, zHeight)
          val newColor = Col.combineWithAlpha(oldColor, color)
          c.wallY1.get.overlayTexture.setRGB(yPos, zHeight, newColor)
        }
      }

      //floors
      if (!c.hasWall && z < overlayPixelSize && c.floor.nonEmpty) {
        val cx = ((x - cposX) * overlaySize).toInt
        val cy = ((y - cposY) * overlaySize).toInt

        val oldColor = c.floor.get.overlayTexture.getRGB(cx, cy)
        val newColor = Col.combineWithAlpha(oldColor, color)
        c.floor.get.overlayTexture.setRGB(cx, cy, newColor)
      }
      //ceils
      if (!c.hasWall && z > 1d - overlayPixelSize && c.ceil.nonEmpty) {
        val cx = ((x - cposX) * overlaySize).toInt
        val cy = ((y - cposY) * overlaySize).toInt

        val oldColor = c.ceil.get.overlayTexture.getRGB(cx, cy)
        val newColor = Col.combineWithAlpha(oldColor, color)
        c.ceil.get.overlayTexture.setRGB(cx, cy, newColor)
      }
    }
  }

  //  def shoot(from: V3, dir: V3, maxDist: Double) = {
  //
  //    def heightAt(dist: Double): Double = (from + dir * dist).z
  //    val poss = iterateOverGrid(from.dropZ, dir.dropZ)
  //      .takeWhile(_.distance(from.dropZ) < maxDist)
  //      .takeWhile(contains)
  //
  //    val hitWall = rayCastFirstWall(from.dropZ, dir.dropZ, maxDist)
  //    hitWall match
  //      case Some(RayCastResult(hitPos, hitCell, distance)) => if(heightAt(hitPos) )
  //      case None => ???
  //
  //
  //  }

}

