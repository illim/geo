package priv

import priv._
import collection.JavaConversions._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import org.lwjgl.util.glu.GLU._

object Repere {

  lazy val (vbo, buff) = {
    val buff = util.BufferUtils.createFloatBuffer(4 * 3)
    val vbo = glGenBuffers()
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    def put(x : Float, y : Float, z : Float) = Seq(x, y, z).foreach(buff.put _)
    put(-100f, 0f, 100f)
    put(100f, 0f, 100f)
    put(100f, 0f, -100f)
    put(-100f, 0f, -100f)
    buff.rewind()
    glBufferData(GL_ARRAY_BUFFER, buff, GL_STREAM_DRAW)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glBindVertexArray(0)
    (vbo, buff)
  }


  def render {
    //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)

    //glColor4f(1f, 0f, 0.0f, 1f)
    /**glEnableVertexAttribArray(0)
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glVertexAttribPointer(0, 3, GL_FLOAT, false, 12, 0)
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
    glDisableVertexAttribArray(0) */

    glBegin(GL_POLYGON)
    glVertex3f(-500f, 0f, 500f)
    glVertex3f(500f, 0f, 500f)
    glColor4f(0.0f, 1f, 00f, 1f)
    glVertex3f(500f, 0f, -500f)
    glVertex3f(-500f, 0f, -500f)
    glEnd()

    //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    glBegin(GL_LINES)
    glColor4f(0.0f, 0f, 1f, 1f)
    glVertex3f(0f, 0f, 0f)
    glVertex3f(0f, 100f, 0f)

    glEnd()
    glClearColor(0f, 0f, 0f, 0.0f)
  }
}
