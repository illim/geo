package priv

import javax.swing._
import java.awt.event._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._

object Main extends JFrame with App {
  val mode = InitDisplay.findDisplayMode(800, 600, 32).get
  val fgc = getGraphicsConfiguration
  val gbounds = fgc.getBounds()
  setSize(mode.getWidth, mode.getHeight + 100)
  setLocation(gbounds.width/2 - getWidth/2, gbounds.height/2 - getHeight / 2 )

  val panel = getContentPane
  panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS))
  val canvas = new java.awt.Canvas()
  val settingsPanel = new SettingsPanel
  panel.add(canvas)
  panel.add(settingsPanel)
  show()
  val g = InitDisplay(canvas, mode)
  canvas.requestFocus()
  var ended = false
  val stats = new Stats
  addWindowListener(new WindowAdapter {
    override def windowClosing(e : WindowEvent) {
      ended = true
      dispose()
    }
  })

  val tree = loadTerrain()

  glPolygonMode(GL_FRONT, GL_LINE)
  var frustum = Frustum(Perspective(90, 800, 600, 10, 2000), Camera(Vec3(500, 250, 600), Vec3(500, 0, 0)))
  updateFrustum(frustum)
  mainLoop()

  def loadTerrain() = {
//    val source = new TextureSource("terrain.png")
    val source = new RawSource("AridRaw.raw")
    val (min, max) = source.minMax
    val hfactor = 300 / (max - min)
    val scale = QuadScale(hfun = { h : Float => (h - min - 100) * hfactor }, xfactor = 10)
    println(s"scale : $scale, min : $min, max : $max")
    (new QuadBuilder(scale = scale)).build(source)
    // (new QuadBuilder(blockSize = 2, hfactor = 2, xfactor = 100)).build(Example2)
  }

  private def updateFrustum(frt : Frustum) = {
    frustum = frt
    tree.root.setVisible(frustum)
    frt()
  }

  def mainLoop() {
    while (!Display.isCloseRequested() && ! ended) {
      if (Display.isVisible()) {
        clearScreen()
        processKeyboard()
        glPushMatrix()
        Repere.render
        glColor4f(1f, 1f, 1f, 1f)
        stats.mon("tree"){
          tree.root.render()
        }
        glPopMatrix()
        Display.update()
        Display.sync(60)
      } else {
        try {
          Thread.sleep(100)
        } catch {
          case _: Throwable =>
        }
      }
    }
    println(stats.report)
    g.cleanUp()
  }

  import org.lwjgl.input.Keyboard
  private def processKeyboard(){
    def cam = frustum.c
    if (Keyboard.isKeyDown(Keyboard.KEY_LEFT)) {
      val newcam = Camera(cam.eye.copy(x = cam.eye.x - 10), cam.center.copy(x = cam.center.x - 10))
      updateFrustum(frustum.copy(c = newcam))
    } else if (Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) {
      val newcam = Camera(cam.eye.copy(x = cam.eye.x + 10), cam.center.copy(x = cam.center.x + 10))
      updateFrustum(frustum.copy(c = newcam))
    } else if (Keyboard.isKeyDown(Keyboard.KEY_DOWN)) {
      val newcam = Camera(cam.eye.copy(z = cam.eye.z + 10), cam.center.copy(z = cam.center.z + 10))
      updateFrustum(frustum.copy(c = newcam))
    }else if (Keyboard.isKeyDown(Keyboard.KEY_UP)) {
      val newcam = Camera(cam.eye.copy(z = cam.eye.z - 10), cam.center.copy(z = cam.center.z - 10))
      updateFrustum(frustum.copy(c = newcam))
    }
  }

  private def clearScreen() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
  }

  class SettingsPanel extends JPanel with ActionListener {
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))
    setSize(800, 100)
    add(new JLabel("yo"))
    def actionPerformed(e : ActionEvent){

    }
  }
}


class Stats {
  import collection._
  private val m = mutable.Map.empty[String, (Long, Int)]

  def mon[A](name : String)(f : => A) = {
    val s = System.currentTimeMillis
    val res = f
    val d = System.currentTimeMillis - s
    m.get(name) match {
      case None => m += (name -> (d, 1))
      case Some((t, c)) => m += (name -> (d + t, c + 1))
    }
    res
  }

  def report = m.toList.map{ case (k, (d, c)) => s"$k : ${(d.toFloat/c)} ($d/$c)"}
}
