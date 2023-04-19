package rraayyss

import java.awt.Color
import scala.util.Random

object Col {
  def multiplyAdd(textureColor: Int, overlayColor: Int, multiplier: Col): Int = {
    val c = new Color(textureColor, true)
    val overlay = new Color(overlayColor, true)
    val oAlpha = overlay.getAlpha / 255f

    new Color(
      multiplier.r * (c.getRed * (1f - oAlpha) + overlay.getRed * oAlpha) / (255f * 255f),
      multiplier.g * (c.getGreen * (1f - oAlpha) + overlay.getGreen * oAlpha) / (255f * 255f),
      multiplier.b * (c.getBlue * (1f - oAlpha) + overlay.getBlue * oAlpha) / (255f * 255f),
    ).getRGB
  }
}


case class Col(r: Int, g: Int, b: Int) {
  def toColor: Color = new Color(r, g, b)
}

def randomColor(seed: Int): Col = {
  val r = new Random(seed)
  Col(r.nextInt(256), r.nextInt(256), r.nextInt(256))
}
