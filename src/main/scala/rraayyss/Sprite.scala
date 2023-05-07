package rraayyss

import utils.math.V2

trait Sprite {
  def lookDir: Option[V2]
  def pos: V2
  def bounds: (V2, V2)
  def colorFunc: V2 => Int
}
