package rraayyss

import utils.math.V2

import java.awt.image.BufferedImage

case class Sprite(
                   faceDir: Option[V2],
                   position: V2,
                   bounds: (V2, V2),
                   tex: BufferedImage) {

  def leftPos(viewDir: V2): V2 = {
    val dir = faceDir match
      case Some(faceDir) => faceDir.rotate90CCW
      case None => viewDir.rotate90CCW

    position + dir * bounds._1.x
  }

  def rightPos(viewDir: V2): V2 = {
    val dir = faceDir match
      case Some(faceDir) => faceDir.rotate90CW
      case None => viewDir.rotate90CW

    position + dir * bounds._2.x
  }

}
