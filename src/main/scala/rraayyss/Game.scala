package rraayyss

import drawing.Drawing
import drawing.core.SimpleDrawable
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.Graphics2D
import java.awt.event.{KeyEvent, KeyListener}

case class GameConfig(

                     )


class Game(
            var config: GameConfig = GameConfig(),
            var position: V2 = V2(3, 3),
            var lookDirection: V2 = V2(1, 1).normalize,
            var map: WorldMap
          ) extends SimpleDrawable {
  Drawing.addKeyBinding(KeyEvent.VK_A, {
    lookDirection = lookDirection.rotate(-0.1)
  })
  Drawing.addKeyBinding(KeyEvent.VK_D, {
    lookDirection = lookDirection.rotate(0.1)
  })

  Drawing.addKeyBinding(KeyEvent.VK_W, {
    position = position + lookDirection * 0.3
  })
  Drawing.addKeyBinding(KeyEvent.VK_S, {
    position = position - lookDirection * 0.3
  })

  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {

  }
}
