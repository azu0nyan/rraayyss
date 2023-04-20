package utils.math


case class V3(x: Double, y: Double, z: Double) {

  @inline def map(f: Double => Double): V3 = V3(f(x), f(y), f(z))

  /** for indexing in cycles */
  @inline def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  @inline def unary_- : V3 = opposite

  @inline def unit: V3 = normalize

  @inline def opposite: V3 = V3(-x, -y, -z)

  @inline def +(v: V3): V3 = V3(x + v.x, y + v.y, z + v.z)

  @inline def -(v: V3): V3 = V3(x - v.x, y - v.y, z - v.z)

  @inline def *(v: V3): V3 = V3(x * v.x, y * v.y, z * v.z)

  @inline def *(s: Double): V3 = V3(x * s, y * s, z * s)

  @inline def /(v: V3): V3 = V3(x / v.x, y / v.y, z / v.z)

  @inline def -(): V3 = V3(-x, -y, -z)

  /** vector(cross) product */
  @inline def ^(v: V3): V3 = V3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

  /** Double product */
  @inline def **(v: V3): Double = x * v.x + y * v.y + z * v.z


  /** fast safe normalization, doing stuff only if needed */
  @inline def normalize: V3 = {
    val l = lengthSquared
    if (l == 1f) return this
    if (l == 0) {
      return V3(0f, 0f, 0f)
    }
    return this / V3(length, length, length)
  }

  /** non oriented angle */
  @inline def angle(v: V3): Double = math.acos(this ** v / (this.length * v.length))

  @inline def distance(v: V3): Double = (this - v).length

  @inline def distanceSquared(v: V3): Double = (this - v).lengthSquared

  //todo
  //def rotate(a: Double) = V3(x * math.cos(a).toDouble - y * math.sin(a).toDouble, x * math.sin(a).toDouble + y * math.cos(a).toDouble)

  @inline def lengthSquared: Double = this ** this

  @inline def length: Double = math.sqrt(lengthSquared)

  @inline def longerThan(ot: V3): Boolean = lengthSquared > ot.lengthSquared

  @inline def shorterThan(ot: V3): Boolean = lengthSquared < ot.lengthSquared

  override def toString: String = s"""V3($x, $y, $z)"""


  /** creating 2d vector dropping x */
  @inline def dropX: V2 = V2(y, z)

  /** creating 2d vector dropping y */
  @inline def dropY: V2 = V2(x, z)

  /** creating 2d vector dropping z */
  @inline def dropZ: V2 = V2(x, y)

  @inline def replaceX(nx: Double): V3 = V3(nx, y, z)

  @inline def replaceY(ny: Double): V3 = V3(x, ny, z)

  @inline def replaceZ(nz: Double): V3 = V3(x, y, nz)

  @inline def toSeq: Seq[Double] = Seq(x, y, z)

  @inline def toArray: Array[Double] = Array(x, y, z)

  @inline def toBasis(basis: (V3, V3, V3)): V3 = toBasis(basis._1, basis._2, basis._3)


  @inline def fromBasis(basis: (V3, V3, V3)): V3 = fromBasis(basis._1, basis._2, basis._3)

  /** (i * x) + (j * y) + (k * z) */
  @inline def fromBasis(i: V3, j: V3, k: V3): V3 = (i * x) + (j * y) + (k * z)


  /** zeroing other axises except x */
  @inline def onlyX: V3 = V3(x, 0, 0)

  /** zeroing x axis */
  @inline def woX: V3 = V3(0, y, z)

  /** zeroing other axises except y */
  @inline def onlyY: V3 = V3(0, y, 0)

  /** zeroing y axis */
  @inline def woY: V3 = V3(x, 0, z)

  /** zeroing other axises except z */
  @inline def onlyZ: V3 = V3(0, 0, z)

  /** zeroing z axis */
  @inline def woZ: V3 = V3(x, y, 0)

  /** return this or -this based on Double product */
  @inline def matchDirection(toMatch: V3): V3 = if (toMatch ** this > 0) this else -this

}

