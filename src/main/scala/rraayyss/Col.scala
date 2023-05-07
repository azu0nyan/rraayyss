package rraayyss

import java.awt.Color
import scala.util.Random

object Col {
  def multiplyAdd(textureColor: Int, overlayColor: Int, multiplier: Col): Int = {
    //blend(tex * mull, over)

    val c = new Color(textureColor, true)
    combineWithAlpha(multiplier.mullColor(c).getRGB, overlayColor)
  }


  /*
  * public Color(int r, int g, int b, int a) {
          value = ((a & 0xFF) << 24) |
                  ((r & 0xFF) << 16) |
                  ((g & 0xFF) << 8)  |
                  ((b & 0xFF) << 0);
  *
  * */
/*
  def combineWithAlpha(col1: Int, col2: Int): Int = {
    //    val c1 = new Color(col1, true)
    //    val c2 = new Color(col2, true)
    val c1a = (col1 >> 24) & 0xFF
    val c1r = (col1 >> 16) & 0xFF
    val c1g = (col1 >> 8) & 0xFF
    val c1b = (col1 >> 0) & 0xFF

    val c2a = (col2 >> 24) & 0xFF
    val c2r = (col2 >> 16) & 0xFF
    val c2g = (col2 >> 8) & 0xFF
    val c2b = (col2 >> 0) & 0xFF

    val c1Alpha = math.min(c1a, 255 - c2a)

    // 200 / 255 * 100 / 255

    val r = (c1r * c1Alpha + c2r * c2a) >> 8
    val g = (c1g * c1Alpha + c2g * c2a) >> 8
    val b = (c1b * c1Alpha + c2b * c2a) >> 8
    val a = math.min(255, c1Alpha + c2a)

    ((a & 0xFF) << 24) |
      ((r & 0xFF) << 16) |
      ((g & 0xFF) << 8) |
      ((b & 0xFF) << 0)
  }*/

    def combineWithAlpha(col1: Int, col2: Int): Int = {
      val c1 = new Color(col1, true)
      val c2 = new Color(col2, true)

      val c1A = math.min(c1.getAlpha / 255f, 1f - c2.getAlpha / 255f)
      val c2A = c2.getAlpha / 255f
      new Color(
        (c1.getRed * c1A + c2.getRed * c2A) / 255f,
        (c1.getGreen * c1A + c2.getGreen * c2A) / 255f,
        (c1.getBlue * c1A + c2.getBlue * c2A) / 255f,
        math.min(1d, c1A + c2A).toFloat
      ).getRGB

    }
}


case class Col(r: Int, g: Int, b: Int) {
  def toColor: Color = new Color(r, g, b)

  def mullColor(c: Color): Color = new Color(
    c.getRed * r / (255f * 255f),
    c.getGreen * g / (255f * 255f),
    c.getBlue * b / (255f * 255f),
  )
}

def randomColor(seed: Int): Col = {
  val r = new Random(seed)
  Col(r.nextInt(256), r.nextInt(256), r.nextInt(256))
}
