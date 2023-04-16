package rraayyss

import java.awt.Color
import scala.util.Random

object Col{
  def mutiply(texCol: Int, col: Col): Int = {
    val c = new Color(texCol)
    new Color(
      col.r * c.getRed / (256f * 256f),
      col.g * c.getGreen / (256f * 256f),
      col.b * c.getBlue / (256f * 256f),
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
