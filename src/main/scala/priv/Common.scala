package priv

import math._
import org.lwjgl.util.vector._
import java.nio.FloatBuffer

object Common {
  @inline def pow2[N](x : N)(implicit num : Numeric[N]) = num.times(x, x)
}

case class Vec3(x : Float, y : Float, z : Float){
  def +(v : Vec3) = Vec3(x + v.x, y + v.y, z + v.z)
  def -(v : Vec3) = Vec3(x - v.x, y - v.y, z - v.z)
  def /(ratio : Float) = Vec3(x / ratio, y / ratio, z / ratio)
  def dot(v : Vec3) = x * v.x +  y * v.y + z * v.z
  def ^(v : Vec3) = Vec3(y * v.z -z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  def *(fact : Float) = Vec3(fact * x, fact * y, fact * z)
  def size = math.sqrt(x * x + y * y + z * z)

  def pvx(v : Vec3) = y * v.z - z * v.y
  def pvy(v : Vec3) = z * v.x - x * v.z
}

object Mat4x4 {
  // ! column major order
  def build(values : Float*) : (FloatBuffer, Matrix4f) = {
    val buff = util.BufferUtils.createFloatBuffer(16)
    values.foreach(buff.put _)
    buff.rewind()
    val m = new Matrix4f
    m.load(buff)
    buff.rewind()
    (buff, m)
  }

  def invert(m : Matrix4f) = {
    val mres = new Matrix4f
    Matrix4f.invert(m, mres)
    mres
  }

  def mult(m : Matrix4f, m2 : Matrix4f) = {
    val mres = new Matrix4f
    Matrix4f.mul(m, m2, mres)
    mres
  }

  def mult(m : Matrix4f, vec : Vec3) = {
    import m._
    import vec._
    val w = m03 * x + m13 * y + m23 * z + m33

    Vec3(
      (m00 * x + m10 * y + m20 * z + m30) / w,
      (m01 * x + m11 * y + m21 * z + m31) / w,
      (m02 * x + m12 * y + m22 * z + m32) / w)
  }
}
