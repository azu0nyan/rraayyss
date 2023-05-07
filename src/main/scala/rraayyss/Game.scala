package rraayyss

import utils.math.V2
import window.{GameWindow, SimpleDrawable}

import java.awt.{Color, Graphics2D}
import java.awt.event.{KeyEvent, KeyListener}
import scala.util.Random

case class GameConfig(
                       speed: Double = 3f,
                       turnSpeed: Double = math.Pi / 2d,
                     )


class Game( window: GameWindow,
            var config: GameConfig = GameConfig(),
            var position: V2 = V2(3, 3),
            var lookDirection: V2 = V2(1, 1).normalize,
            var map: WorldMap
          ) extends SimpleDrawable {
  var leftPressed = false
  var rightPressed = false
  var forwardPressed = false
  var backPressed = false


  window.addKeyBinding(KeyEvent.VK_A, {
    leftPressed = true
  }, true)
  window.addKeyBinding(KeyEvent.VK_A, {
    leftPressed = false
  }, false)
  window.addKeyBinding(KeyEvent.VK_D, {
    rightPressed = true
  }, true)
  window.addKeyBinding(KeyEvent.VK_D, {
    rightPressed = false
  }, false)
  window.addKeyBinding(KeyEvent.VK_W, {
    forwardPressed = true
  }, true)
  window.addKeyBinding(KeyEvent.VK_W, {
    forwardPressed = false
  }, false)
  window.addKeyBinding(KeyEvent.VK_S, {
    backPressed = true
  }, true)
  window.addKeyBinding(KeyEvent.VK_S, {
    backPressed = false
  }, false)

  window.addKeyBinding(KeyEvent.VK_SPACE, {
    val rcr = map.rayCastFirstWall(position, lookDirection.rotate(new Random().nextDouble() * 0.1 - 0.05), 300)
    rcr match
      case Some(RayCastResult(hitPos, hitCell, distance)) =>
        map.explode(hitPos.addZ(0.5 + new Random().nextDouble() * 0.2 - 0.2), 0.24, new Color(2,22,222, 120).getRGB)
      case None =>
  }, false)


  override def drawAndUpdate(g: Graphics2D, dt: Double): Unit = {
    val fwd = (if (forwardPressed) 1 else 0) + (if (backPressed) -1 else 0)
    val lr = (if (leftPressed) -1 else 0) + (if (rightPressed) 1 else 0)

    lookDirection = lookDirection.rotate(lr * dt * config.turnSpeed)
    position = position + lookDirection * fwd * dt * config.speed

    if(fwd != 0) {
      map.explode(position.addZ(0d), 0.25, new Color(10, 150, 10, 120).getRGB)
    }
  }
}
