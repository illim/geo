package priv

import org.lwjgl.input.Keyboard
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.util.glu.GLU._

object InitDisplay {

  def findDisplayMode(width: Int, height: Int, bpp: Int) = {
    val modes = Display.getAvailableDisplayModes()
    modes.find { mode =>
      (mode.getWidth() == width && mode.getHeight() == height && mode.getBitsPerPixel() >= bpp)
    }
  }

  def apply(canvas : java.awt.Canvas, mode : DisplayMode) = {
    Display.setDisplayMode(mode)
    Display.setParent(canvas)
    Display.create()
    Display.setVSyncEnabled(true)
    initGLState(mode)
    DisplayConf(Display.getWidth, Display.getHeight)
  }

  private def initGLState(mode: DisplayMode) {
    def initDepth() {
      glClearDepth(1.0f)
      glEnable(GL_DEPTH_TEST)
      glDepthFunc(GL_LEQUAL)
    }
    Keyboard.create()
    initDepth()
    //glEnable(GL_ALPHA_TEST)
    //glEnable(GL_BLEND)
    glClearColor(0f, 0f, 0f, 0.5f) //set clear color to black
    glViewport(0, 0, mode.getWidth(), mode.getHeight)
    org.lwjgl.opengl.Util.checkGLError()
  }
}

case class DisplayConf(val width: Int, val height: Int) {
  def cleanUp() {
    Display.destroy()
  }
}
