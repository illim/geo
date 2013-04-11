package priv

import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.util.glu.GLU._
import org.lwjgl.util.vector._

case class Perspective(fovx : Float, resx : Int, resy : Int, near : Float, far : Float){
  val aspect = resx.toFloat / resy
  val fovy = fovx / aspect
  val fovxr = math.toRadians(fovx)
  val fovyr = math.toRadians(fovy)
  val f = (1/math.tan(fovyr / 2)).toFloat
  val m = getMatrix()
  val M = m._2
  m._1.rewind()

  def apply(){
    glMultMatrix(m._1)
    //gluPerspective(fovy, aspect, near, far)
  }

  private def getMatrix() = {
    val a = (near + far) / (near - far)
    val b = (2f * far * near) / (near - far)
    Mat4x4.build(
      f/aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, a, -1,
      0, 0, b, 0)
  }
}

case class Camera(eye : Vec3, center : Vec3){
  val m = getMatrix()
  val M = m._2
  m._1.rewind()

  def apply(){
    glMultMatrix(m._1)
    glTranslatef(-eye.x,- eye.y,- eye.z)
   /**
    gluLookAt(eye.x, eye.y, eye.z, center.x, center.y, center.z, 0, 1, 0)*/
  }

  private def getMatrix() = {
    val F = center - eye
    val UP = Vec3(0, 1, 0)
    val f = F / F.size.toFloat
    val s = f ^ UP
    val u = s ^ f
    Mat4x4.build(
      s.x, u.x, -f.x, 0,
      s.y, u.y, -f.y, 0,
      s.z, u.z, -f.z, 0,
      0, 0, 0, 1)
  }
}

case class Frustum(p : Perspective, c : Camera){
  val m = Mat4x4.mult(p.M, c.M)

  def project(v : Vec3) = {
    Mat4x4.mult(p.M , Mat4x4.mult(c.M , v - c.eye))
  }

  def isVisible(v : Vec3) = {
    val pv = project(v)
    val dx = 1f - math.abs(pv.x)
    val dy = 1f - math.abs(pv.y)
    val dz = 1f - math.abs(pv.z)
    dx > 0 && dy > 0 && dz > 0
  }

  import Common._
  // v projected
  def dist(v : Vec3) = {
    val dx = 1f - math.abs(v.x)
    val dy = 1f - math.abs(v.y)
    val dz = 1f - math.abs(v.z)
    if (dx > 0 && dy > 0 && dz > 0) 0 else {
      math.sqrt(pow2(dx) + pow2(dy) + pow2(dz))
    }
  }

  def apply() {
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    p()
    c()
  }

  def unproject(v : Vec3) = {
    val mi = Mat4x4.invert(m)
    Mat4x4.mult(mi, v)
  }
}
