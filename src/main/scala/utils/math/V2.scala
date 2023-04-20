package utils.math

import utils.math.V3



object V2 {
  val ox: V2 = V2(1, 0)
  val oy: V2 = V2(0, 1)
  val ZERO : V2= V2(0, 0)
}

case class V2(x: Double, y: Double){
  def toProduct:(Double, Double) = (x, y)



  @inline def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  @inline def xInt:Int = x.toInt

  @inline def yInt:Int = y.toInt

  @inline def unary_- : V2 = opposite

  @inline def opposite: V2 = V2(-x, -y)

  @inline def +(v: V2): V2 = V2(x + v.x, y + v.y)

  @inline def -(v: V2): V2 = V2(x - v.x, y - v.y)

  @inline def *(v: V2): V2 = V2(x * v.x, y * v.y)

  @inline def *(s:Double): V2 = V2(x * s, y * s)

  @inline def /(v: V2): V2 = V2(x / v.x, y / v.y)

  @inline def -(): V2 = V2(-x, -y)

  @inline def **(v: V2): Double = x * v.x + y * v.y

  @inline def det(v: V2): Double = x * v.y - y * v.x

  @inline def normalize: V2 = if (length == 0) {
    V2(0, 0)
  } else {
    this / V2(length, length)
  }

  @inline def angleToOX: Double =  math.atan2(y, x)

  @inline def angle(v: V2): Double = (math.atan2(v.y, v.x) - math.atan2(y, x))


  /**Y - up, returns from - PI to PI*/
  def angleCCW(ot: V2): Double = math.atan2(det(ot), this ** ot)


  @inline def distance(v: V2): Double = (this - v).length

  @inline def rotate90CCW:V2 = V2(-y, x)

  @inline def rotate90CW:V2 = V2(y, -x)

  @inline def rotate(a: Double):V2 = V2(x * math.cos(a) - y * math.sin(a), x * math.sin(a) + y * math.cos(a))

  @inline   def rotateAroundPoint(rotation: Double, point: V2): V2 = (this - point).rotate(rotation) + point

  @inline def scaleAroundPoint(scale: Double, point: V2): V2 = (this - point) * scale + point

  @inline def lengthSquared:Double = this ** this

  @inline def length: Double = math.hypot(x, y)

  override def toString: String = s"""V2($x, $y)"""


  @inline def addZ(z: Double):V3 = V3(x, y, z)

  @inline def addX(X: Double):V3 = V3(X, x, y)

  @inline def addY(Y: Double):V3 = V3(x, Y, y)

  @inline def toSeq: Seq[Double] = Seq(x, y)

  @inline def planarToV3(upCord: Double): V3 = V3(x, upCord, -y)



}

class UnitV2(v: V2) extends V2(v.normalize.x, v.normalize.y)
