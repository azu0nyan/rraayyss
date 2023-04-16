package rraayyss
import utils.heightmap.PerlinNoise
import utils.math.planar.V2

import java.awt.Color
import scala.util.Random

case class Col(r: Int, g: Int, b: Int) {
  def toColor: Color = new Color(r, g, b)
}

def randomColor(seed: Int): Col = {
  val r = new Random(seed)
  Col(r.nextInt(256), r.nextInt(256), r.nextInt(256))
}

sealed trait Cell
case object Empty extends Cell
case class Wall(c: Col) extends Cell

case class RayCastResult(
                          hitPos: V2,
                          hitWall: Wall,
                          distance: Double
                        )
object WorldMap{
  def fromPerlinNoise(size: V2): WorldMap = {
    def cFunc(x: Int)(y: Int): Cell = {
      val n = PerlinNoise.noise(x / 3f, y / 3f, 1f)
      if (n > 0.5) Wall(randomColor(x * 11231232 + y * 1111111))
      else Empty
    }

    WorldMap(xSize = size.xInt, ySize = size.yInt, cell = cFunc)
  }  
}



case class WorldMap(
                     xSize: Int = 16,
                     ySize: Int = 16,
                     cellSize: Int = 1,
                     cell: Int => Int => Cell /*=
                     x => y =>
                       if (new Random(x * 11231232 + y * 1111111).nextInt(100) == 0)
                         Wall(randomColor(x * 11231232 + y * 1111111))
                       else Empty*/
                   ) {

  def indices: Iterator[(Int, Int)] = for (x <- (0 until xSize).iterator; y <- (0 until ySize).iterator) yield (x, y)

  def cellPos(x: Double, y: Double): (Int, Int) = ((x / cellSize).floor.toInt, (y / cellSize).floor.toInt)

  def cellAt(x: Double, y: Double): Option[Cell] = {
    val (xPos, yPos) = cellPos(x, y)
    Option.when(xPos < xSize && xPos >= 0 && yPos < ySize && yPos >= 0)(cell(xPos)(yPos))
  }

  def iterateOverGrid(from: V2, dir: V2, eps: Double = 0.000001d): Iterator[V2] = new Iterator[V2] {
    var t = 0d

    override def hasNext: Boolean = true
    override def next(): V2 = {
      val curPos = from + dir * t
      var (tx, ty) = cellPos(curPos.x, curPos.y)
      var inX = curPos.x - tx * cellSize
      var inY = curPos.y - ty * cellSize

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


  def rayCast(from: V2, dir: V2, maxLength: Double): Option[RayCastResult] = {
    val posW = iterateOverGrid(from, dir).takeWhile(_.distance(from) <= maxLength).collect(pos => cellAt(pos.x, pos.y) match
      case Some(w@Wall(_)) => (pos, w)
    ).toSeq.headOption

    posW.map(pos => RayCastResult(pos._1, pos._2, pos._1.distance(from)))
  }
}
