import drawing.core.SimpleDrawable
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.{BasicStroke, Color, Graphics2D}

case class RenderParams(
                         fov: Double = Math.PI / 3d,
                         maxDist: Double = 300d,
                         heightAtMax: Double = 1,
                         heightAtMin: Double = 600,
                         bgColor: Color = Color.CYAN,
                         lt: V2 = V2(100, 100),
                         size: V2  = V2(1000, 600)

                       )

class Render(
              game: Game,
              params: RenderParams = RenderParams()
            ) extends SimpleDrawable {
  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    g.setColor(Color.BLACK)
    g.fillRect(100, 100, System.currentTimeMillis().toInt % 1920.toInt, 300)

    renderMiniMap(g, V2(1100, 100), V2(700, 700))
    renderGame(g, params.lt, params.size)
  }


  def renderGame(g: Graphics2D, lt: V2, size: V2): Unit = {
    g.setColor(new Color(122, 20, 200))
    g.fillRect(lt.xInt, lt.yInt, size.xInt, size.yInt)

    for (dx <- 0 until size.xInt) {
      val x = lt.xInt + dx
      val dir = game.lookDirection.rotate(-params.fov / 2d + dx / size.x * params.fov)
      val rc = game.map.rayCast(game.position, dir, params.maxDist)
      rc match
        case Some(RayCastResult(hitPos, hitWall, distance)) =>
          g.setStroke(new BasicStroke(1))
          g.setColor(hitWall.c.toColor)
//          val height = params.heightAtMin + distance / params.maxDist * (params.heightAtMax - params.heightAtMin)
          val height = params.size.y / distance
          g.drawLine(x, (lt.y + size.y / 2d - height / 2d).toInt , x, (lt.y + size.y / 2d + height / 2d).toInt)
        case None =>
//          g.setStroke(new BasicStroke(1))
//          g.setColor(params.bgColor)
//          g.drawLine(x, 0, x, 1080)


    }
  }


  def renderMiniMap(g: Graphics2D, lt: V2, size: V2): Unit = {
    g.setColor(new Color(12, 109, 203))
    g.fillRect(lt.xInt, lt.yInt, size.xInt, size.yInt)

    def toMapPos(pos: V2): (Int, Int) = {
      val x = lt.x + (pos.x / (game.map.xSize * game.map.cellSize)) * size.x
      val y = lt.y + (pos.y / (game.map.ySize * game.map.cellSize)) * size.y
      (x.toInt, y.toInt)
    }

    def drawCell(cx: Int, cy: Int, color: Color): Unit = {
      g.setColor(color)
      val whx = size.x / game.map.xSize
      val why = size.y / game.map.ySize

      val x = lt.x + whx * cx
      val y = lt.y + why * cy

      g.fillRect(x.toInt, y.toInt, whx.toInt, why.toInt)
    }
    for ((x, y) <- game.map.indices) {
      val c = game.map.cell(x)(y)
      c match
        case Empty =>
        case Wall(c) => drawCell(x, y, c.toColor)
    }


    for (pos <- game.map.iterateOverGrid(game.position, game.lookDirection).take(30).toSeq) {
      val (cx, cy) = game.map.cellPos(pos.x, pos.y)
      drawCell(cx, cy, new Color(255, 0, 0, 100))
    }

    val (fx, fy) = toMapPos(game.position)

    val (tmx, tmy) = toMapPos(game.position + game.lookDirection * params.maxDist)
    g.setStroke(new BasicStroke(3))
    g.setColor(Color.GREEN)
    g.drawLine(fx, fy, tmx, tmy)
    val (tlx, tly) = toMapPos(game.position + game.lookDirection.rotate(params.fov /2d) * params.maxDist)
    val (trx, try_) = toMapPos(game.position + game.lookDirection.rotate(-params.fov /2d) * params.maxDist)
    g.setStroke(new BasicStroke(1))
    g.drawLine(fx, fy, tlx, tly)
    g.drawLine(fx, fy, trx, try_)



    val rc = game.map.rayCast(game.position, game.lookDirection, params.maxDist)
    for (RayCastResult(hit, w, dist) <- rc) {
      g.setColor(Color.RED)
      val (tx, ty) = toMapPos(hit)
      g.drawLine(fx, fy, tx, ty)
    }


  }
}
