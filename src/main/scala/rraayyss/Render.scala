package rraayyss

import rraayyss.Game
import utils.math.V2
import window.SimpleDrawable

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import java.util.concurrent.{CountDownLatch, Executors}
import javax.imageio.ImageTypeSpecifier
import scala.concurrent.duration.{Duration, pairIntToDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

case class RenderParams(
                         fov: Double = Math.PI / 3d,
                         maxDist: Double = 300d,
                         heightAtMax: Double = 1,
                         heightAtMin: Double = 600,
                         bgColor: Color = Color.CYAN,
                         lt: V2 = V2(100, 100),
                         size: V2 = V2(1000, 600),
                         minimaplt: V2 = V2(100, 100),
                         minimapSize: V2 = V2(1000, 600),
                         threads: Int = 10,
                       )

class Render(
              game: Game,
              params: RenderParams = RenderParams()
            ) extends SimpleDrawable {
  println(s"Created renders with ${params.threads} threads.")

  //  val fjp = new ForkJoinPool(params.threads)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(params.threads))

  override def drawAndUpdate(g: Graphics2D, dt: Double): Unit = {
    g.setColor(Color.BLACK)
    g.fillRect(100, 100, System.currentTimeMillis().toInt % 1920.toInt, 300)


    //    renderMiniMap(g, params.minimaplt, params.minimapSize)
    //    renderGame(g, params.lt, params.size)
    val f1 = Future(renderMiniMap(g, params.minimaplt, params.minimapSize))
    val f2 = Future(renderGame(g, params.lt, params.size))

    Await.result(f1, Duration.Inf)
    Await.result(f2, Duration.Inf)

  }


  def fillVertical(in: BufferedImage, x: Int, minY: Int, maxY: Int,
                   colorFromFract: Double => Int = c => Color.BLACK.getRGB)(depthMin: Double, depthMax: Double, depthBuffer: Array[Double]): Unit = {
    for (y <- math.max(0, minY) until math.min(maxY, in.getHeight)) {
      val pct = (y - minY).toDouble / (maxY - minY).toDouble
      val col = colorFromFract(pct)
      val depth = depthMin + (depthMax - depthMin) * (y - minY) / (maxY - minY)
      if (depthBuffer(y * in.getWidth + x) < depth) {
        depthBuffer(y * in.getWidth + x) = depth
        val finalCol: Int = Col.combineWithAlpha(in.getRGB(x, y), col)
        in.setRGB(x, y, finalCol)
      }
    }
  }


  def heightAtDist(screenMax: Double, dist: Double, dirAngleOffset: Double): Double = screenMax / (dist * math.cos(dirAngleOffset))

  def posAtDist(screenMax:Double, dist: Double, dirAngleOffset: Double, pos: Double): Double = screenMax / 2d  - heightAtDist(screenMax, dist, dirAngleOffset) * pos / 2d

  def renderGameWorld(width: Int, height: Int): BufferedImage = {

    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val depthBuffer = Array.fill(width * height)(Double.MinValue)

    val gr = bi.getGraphics
    gr.setColor(Color.BLUE)
    gr.fillRect(0, 0, width, height)

    def renderRay(x: Int): Unit = {
      val dirAngleOffset = -params.fov / 2d + x.toDouble / width * params.fov
      val dir = game.lookDirection.rotate(dirAngleOffset)
//      def wHeight(dist: Double): Double = heightAtDist(height, dist, dirAngleOffset)
//      def wallBot(dist: Double): Int = (height / 2d + wHeight(dist) / 2d).toInt
//      def wallTop(dist: Double): Int = (height / 2d - wHeight(dist) / 2d).toInt

      def wallBot(dist:Double):Int = posAtDist(height, dist, dirAngleOffset, -1).toInt
      def wallTop(dist:Double):Int = posAtDist(height, dist, dirAngleOffset, 1).toInt

      val rc = game.map.rayCastAllTiles(game.position, dir, params.maxDist)
      val tId = rc.indexWhere(_.hitCell.hasWall)


      if (tId >= 1) {
        for (Seq(f, s) <- rc.take(tId + 1).sliding(2)) {
          val secondHit = s.hitPos
          val firstHit = f.hitPos
          val body = firstHit - secondHit

          if (f.hitCell.floor.nonEmpty) {
            val floor = f.hitCell.floor.get
            def cFuncFloor(pct: Double): Int = {
              val nPct = if (pct < 0.01) 0.01 else if (pct > 0.99) 0.99 else pct //hack to compensate rayCasting offset todo fix
              val p = secondHit + body * nPct
              val u = p.x - p.x.floor
              val v = p.y - p.y.floor
              val cAt = TextureLibrary.colorAt(floor.texture, u, v)
              val oAt = TextureLibrary.colorAt(floor.overlayTexture, u, v)
              val col = Col.multiplyAdd(cAt, oAt, floor.colorMultiplier)
              col
            }

            fillVertical(bi, x, wallBot(s.distance), wallBot(f.distance), cFuncFloor)(s.distance, f.distance, depthBuffer)
          }
          if (f.hitCell.ceil.nonEmpty) {
            val ceil = f.hitCell.ceil.get
            def cFuncCeil(pct: Double): Int = {
              val nPct = if (pct < 0.01) 0.01 else if (pct > 0.99) 0.99 else pct //hack to compensate rayCasting offset todo fix
              val p = firstHit + (secondHit - firstHit) * nPct
              val u = p.x - p.x.floor
              val v = p.y - p.y.floor
              val cAt = TextureLibrary.colorAt(ceil.texture, u, v)
              val oAt = TextureLibrary.colorAt(ceil.overlayTexture, u, v)
              Col.multiplyAdd(cAt, oAt, ceil.colorMultiplier)
            }

            fillVertical(bi, x, wallTop(f.distance), wallTop(s.distance), cFuncCeil)(f.distance, s.distance, depthBuffer)
          }
        }
      }
      //paint wall

      if (tId >= 0 && rc(tId).hitCell.hasWall) {
        val hitResult = rc(tId)
        val cell = hitResult.hitCell
        val eps = 0.0005d


        val (cposX, cposY) = game.map.cellPos(hitResult.hitPos.x, hitResult.hitPos.y)

        val wallSide: TexturedPlane =
          if (hitResult.hitPos.x - cposX < eps) cell.wallX.get
          else if (hitResult.hitPos.x - cposX > 1 - eps) cell.wallX1.get
          else if (hitResult.hitPos.y - cposY < eps) cell.wallY.get
          else if (hitResult.hitPos.y - cposY > 1 - eps) cell.wallY1.get
          else {
            //            throw new Exception(s"Can't detect side ${hitResult.hitPos.x - cposX} ${hitResult.hitPos.y - cposY}") //todo fix
            cell.wallX.get
          }


        val dPos = hitResult.hitPos.x - hitResult.hitPos.x.floor + hitResult.hitPos.y + hitResult.hitPos.y.floor
        def cFunc(y: Double): Int =
          val cAt = TextureLibrary.colorAt(wallSide.texture, dPos, y)
          val oAt = TextureLibrary.colorAt(wallSide.overlayTexture, dPos, y)
          Col.multiplyAdd(cAt, oAt, wallSide.colorMultiplier)


        fillVertical(bi, x, wallTop(hitResult.distance), wallBot(hitResult.distance), cFunc)(hitResult.distance, hitResult.distance, depthBuffer)
      }

    }

    def renderSprite(s: Sprite): Unit = {
        val toSprite = s.position - game.position
        if(game.lookDirection ** toSprite >= 0) {
          val lpos = s.leftPos(game.lookDirection)
          val toLpos = lpos - game.position
          val toLAngle = game.lookDirection.angleCCW(toLpos)
          val onScreenLeft = (toLAngle - params.fov / 2)/ (params.fov ) * (width )

          val rpos = s.rightPos(game.lookDirection)
          val toRpos = rpos - game.position
          val toRAngle = game.lookDirection.angleCCW(toRpos)
          val onScreenRight = (toRAngle - params.fov / 2) / (params.fov) * (width)

          val bot = posAtDist(height, toSprite.length, toLAngle, s.bounds._1.y)
          val top = posAtDist(height, toSprite.length, toRAngle, s.bounds._2.y)

          bi.getGraphics.drawImage(s.tex, math.min(onScreenLeft, onScreenRight).toInt, bot.toInt, math.abs(onScreenLeft - onScreenRight).toInt, math.abs(bot - top).toInt, null)

        }
    }

    //с CountDownLatch на 2 FPS больше чем с
    // Await.result(Future.sequence(futs), Duration.Inf)
    val ccdRays = new CountDownLatch(width)
    val futs = for (x <- 0 until width) yield Future {
      renderRay(x)
      ccdRays.countDown()
    }
    ccdRays.await()


    val sp = game.sprites
    val ccdSprites = new CountDownLatch(sp.size)
    for (s <- sp) yield Future {
      renderSprite(s)
      ccdSprites.countDown()
    }
    ccdSprites.await()
    bi
  }

  def renderGame(g: Graphics2D, lt: V2, size: V2): Unit = {
    val bi = renderGameWorld(size.xInt, size.yInt)
    g.drawImage(bi, lt.xInt, lt.yInt, null)
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
      if (c.hasWall) {
        drawCell(x, y, c.wallX.get.colorMultiplier.toColor)
      } else if (c.floor.nonEmpty) {
        drawCell(x, y, c.floor.get.colorMultiplier.toColor)
      }
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

