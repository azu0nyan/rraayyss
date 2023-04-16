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
                          hitWall: Cell,
                          distance: Double
                        )
object WorldMap{
  def fromPerlinNoise(size: V2): WorldMap = {
    def cFunc(x: Int)(y: Int): Cell = {
      val n = PerlinNoise.noise(x / 3f, y / 3f, 1f)
      if (x == 0 || y == 0 || x == (size.x.toInt - 1) || y == (size.y.toInt - 1) || n > 0.5) Wall(randomColor(x * 11231232 + y * 1111111))
      else Empty
    }

    WorldMap(xSize = size.xInt, ySize = size.yInt, cell = cFunc)
  }
}



case class WorldMap(
                     xSize: Int = 16,
                     ySize: Int = 16,
                     cellSize: Int = 1,
                     cell: Int => Int => Cell
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




  def rayCastAllTiles(from: V2, dir: V2, maxLength: Double): Seq[RayCastResult] = {
    iterateOverGrid(from, dir).takeWhile(_.distance(from) <= maxLength).takeWhile(contains).map{case v@V2(x, y) => RayCastResult(v, cellAt(x, y).get, v.distance(from))}.toSeq
  }
  def rayCastFirstWall(from: V2, dir: V2, maxLength: Double): Option[RayCastResult] = {
    val posW = iterateOverGrid(from, dir).takeWhile(_.distance(from) <= maxLength).collect(pos => cellAt(pos.x, pos.y) match
      case Some(w@Wall(_)) => (pos, w)
    ).toSeq.headOption

    posW.map(pos => RayCastResult(pos._1, pos._2, pos._1.distance(from)))
  }
}

