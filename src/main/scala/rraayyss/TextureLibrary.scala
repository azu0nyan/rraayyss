package rraayyss

import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.language.postfixOps

object TextureLibrary {

  var lib: mutable.Map[String, BufferedImage] = mutable.Map()


  def loadTexture(name: String): Option[BufferedImage] = try {
    val img = ImageIO.read(Path.of("data", "sprites", name).toFile)
    lib += name -> img
    Some(img)
  } catch {
    case e: IOException =>
      e.printStackTrace()
      None
  }

  def getTexture(name: String): Option[BufferedImage] = {
    lib.get(name) match
      case Some(value) => Some(value)
      case None => loadTexture(name)
  }
  
  def colorAt(tex: BufferedImage, u: Double, v: Double): Int = {
    val x = ((u % 1d + 1d) %1d) * tex.getWidth toInt 
    val y = (1d - ((v % 1d + 1d) %1d)) * tex.getHeight toInt
    
    tex.getRGB(x, y)
  } 
  
  def colorAt(name: String, u: Double, v: Double): Int =
    colorAt(getTexture(name).get, u, v)
}
