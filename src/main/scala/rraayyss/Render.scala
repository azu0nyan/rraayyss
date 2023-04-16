package rraayyss

import drawing.core.SimpleDrawable
import rraayyss.Game
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import javax.imageio.ImageTypeSpecifier

case class RenderParams(
                         fov: Double = Math.PI / 3d,
                         maxDist: Double = 300d,
                         heightAtMax: Double = 1,
                         heightAtMin: Double = 600,
                         bgColor: Color = Color.CYAN,
                         lt: V2 = V2(100, 100),
                         size: V2 = V2(1000, 600)

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


  def fillVertical(in: BufferedImage, x: Int, minY: Int, maxY: Int, colorFromFract: Double => Int = c => Color.BLACK.getRGB): Unit = {
    for (y <- math.max(0, minY) until math.min(maxY, in.getHeight)) {
      val pct = (y - minY).toDouble / (maxY - minY).toDouble
      val col = colorFromFract(pct)
      in.setRGB(x, y, col)
    }
  }

  def renderGameWorld(width: Int, height: Int): BufferedImage = {

    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val gr = bi.getGraphics
    gr.setColor(Color.RED)
    gr.fillRect(0, 0, width, height)

    for (x <- 0 until width) {
      val dirAngleOfset = -params.fov / 2d + x.toDouble / width * params.fov
      val dir = game.lookDirection.rotate(dirAngleOfset)
      def wHeight(dist: Double): Double = params.size.y / (dist * math.cos(dirAngleOfset))
      def wBot(dist: Double): Int = (height / 2d + wHeight(dist) / 2d).toInt
      def wTop(dist: Double): Int = (height / 2d - wHeight(dist) / 2d).toInt

      val rc = game.map.rayCastAllTiles(game.position, dir, params.maxDist)
      val tId = rc.indexWhere(_.hitCell.isInstanceOf[Wall])
//      println(rc.size + " " + tId + " " + rc.count(_.hitCell.isInstanceOf[Wall]) +" " + rc.count(_.hitCell.isInstanceOf[EmptyCell.type ]) + " " + " " + rc.count(_.hitCell.isInstanceOf[Floor])  )
      //      if(rc.size > 100) {
      //        println(rc.size)
      //        println(game.position)
      //        println(dir)
      //        println(params.maxDist)
      //        println(rc)
      //      }
      if (tId >= 1) {
        for (Seq(f, s) <- rc.take(tId + 1).sliding(2)) {
          val bot = s.hitPos
          val top = f.hitPos
          f.hitCell match
            case Floor(c, tex) =>
              def cFuncFloor(pct: Double): Int = tex match
                case Some(texName) =>
                  val p = bot + (top - bot) * pct
                  val u = p.x - p.x.floor
                  val v = p.y - p.y.floor
                  val cAt = TextureLibrary.colorAt(texName, u, v)
                  Col.mutiply(cAt, c)
                case None =>
                  c.toColor.getRGB

              fillVertical(bi, x, wBot(s.distance), wBot(f.distance), cFuncFloor)

              def cFuncCeil(pct: Double): Int = tex match
                case Some(texName) =>
                  val p = bot + (top - bot) * pct
                  val u = p.x - p.x.floor
                  val v = p.y - p.y.floor
                  val cAt = TextureLibrary.colorAt(texName, u, v)
                  Col.mutiply(cAt, c)
                case None =>
                  c.toColor.getRGB
              fillVertical(bi, x, wTop(f.distance), wTop(s.distance), cFuncCeil)

            case _ =>
        }
      }
      if (tId >= 0) {
        val w = rc(tId).hitCell.asInstanceOf[Wall]
        val dPos = rc(tId).hitPos.x - rc(tId).hitPos.x.floor + rc(tId).hitPos.y + rc(tId).hitPos.y.floor
        def cFunc(y: Double): Int =
          w.tex match
            case Some(value) =>
              val cAt = TextureLibrary.colorAt(value, dPos, y)
              Col.mutiply(cAt, w.c)
            case None => w.c.toColor.getRGB

        fillVertical(bi, x, wTop(rc(tId).distance), wBot(rc(tId).distance), cFunc)
      }

      /*Single wall rendering*/
      //      val rc = game.map.rayCastFirstWall(game.position, dir, params.maxDist)
      //      rc match
      //        case Some(RayCastResult(hitPos, hitWall, distance)) =>
      //          val dPos = hitPos.x - hitPos.x.floor + hitPos.y + hitPos.y.floor
      //          def cFunc(y: Double): Int = TextureLibrary.colorAt("billy_pixel_1.png", dPos, y)
      //          val wHeight = params.size.y / (distance * math.cos(dirAngleOfset))
      //          fillVertical(bi, x, (height / 2d - wHeight / 2d).toInt, (height / 2d + wHeight / 2d).toInt, cFunc)
      //        case None =>
      //
    }
    bi
  }

  def renderGame(g: Graphics2D, lt: V2, size: V2): Unit = {
    val bi = renderGameWorld(size.xInt, size.yInt)
    g.drawImage(bi, lt.xInt, lt.yInt, null)
  }
  /*
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
    }*/


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
        case EmptyCell =>
        case Wall(c, _) => drawCell(x, y, c.toColor)
        case Floor(c, _) => drawCell(x, y, c.toColor)
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
    val (tlx, tly) = toMapPos(game.position + game.lookDirection.rotate(params.fov / 2d) * params.maxDist)
    val (trx, try_) = toMapPos(game.position + game.lookDirection.rotate(-params.fov / 2d) * params.maxDist)
    g.setStroke(new BasicStroke(1))
    g.drawLine(fx, fy, tlx, tly)
    g.drawLine(fx, fy, trx, try_)


    val rc = game.map.rayCastFirstWall(game.position, game.lookDirection, params.maxDist)
    for (RayCastResult(hit, w, dist) <- rc) {
      g.setColor(Color.RED)
      val (tx, ty) = toMapPos(hit)
      g.drawLine(fx, fy, tx, ty)
    }


  }
}

