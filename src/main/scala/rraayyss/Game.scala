package rraayyss

import drawing.Drawing
import drawing.core.SimpleDrawable
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.Graphics2D
import java.awt.event.{KeyEvent, KeyListener}

case class GameConfig(
                       speed: Double = 3f,
                       turnSpeed: Double = math.Pi / 2d,
                     )


class Game(
            var config: GameConfig = GameConfig(),
            var position: V2 = V2(3, 3),
            var lookDirection: V2 = V2(1, 1).normalize,
            var map: WorldMap
          ) extends SimpleDrawable {
  var leftPressed = false
  var rightPressed = false
  var forwardPressed = false
  var backPressed = false


  Drawing.addKeyBinding(KeyEvent.VK_A, {
    leftPressed = true
  }, true)
  Drawing.addKeyBinding(KeyEvent.VK_A, {
    leftPressed = false
  }, false)
  Drawing.addKeyBinding(KeyEvent.VK_D, {
    rightPressed = true
  }, true)
  Drawing.addKeyBinding(KeyEvent.VK_D, {
    rightPressed = false
  }, false)
  Drawing.addKeyBinding(KeyEvent.VK_W, {
    forwardPressed = true
  }, true)
  Drawing.addKeyBinding(KeyEvent.VK_W, {
    forwardPressed = false
  }, false)
  Drawing.addKeyBinding(KeyEvent.VK_S, {
    backPressed = true
  }, true)
  Drawing.addKeyBinding(KeyEvent.VK_S, {
    backPressed = false
  }, false)


  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    val fwd = (if (forwardPressed) 1 else 0) + (if (backPressed) -1 else 0)
    val lr = (if (leftPressed) -1 else 0) + (if (rightPressed) 1 else 0)

    lookDirection = lookDirection.rotate(lr * dt * config.turnSpeed)
    position = position + lookDirection * fwd * dt * config.speed
  }
}
