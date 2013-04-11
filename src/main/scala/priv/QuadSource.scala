package priv

import javax.imageio.ImageIO
import javax.imageio.stream._
import org.newdawn.slick.util.ResourceLoader

trait QuadSource {
  def size : Int
  def read(x : Int, y : Int, blockSize : Int) : Array[Float]
  def minMax : (Float, Float)
}

class TextureSource(name : String) extends QuadSource {
  private val buffImage = ImageIO.read(new MemoryCacheImageInputStream(ResourceLoader.getResourceAsStream(name)))
  val size = buffImage.getHeight -1
  val raster = buffImage.getRaster()
//  assert(size == buffImage.getWidth -1)

  val minMax = {
    val data = raster.getDataElements(0, 0, buffImage.getWidth, buffImage.getHeight, null).asInstanceOf[Array[Short]]
    val first = toFloat(data(0))
    ((first, first) /: data){ case ((min, max), c) =>
      val elt = toFloat(c)
      (if (elt < min) elt else min, if (elt > max) elt else max)
    }
  }

  def read(x : Int, y : Int, blockSize : Int) = {
    raster.getDataElements(x, y, blockSize +1, blockSize +1, null).asInstanceOf[Array[Short]].map(toFloat _)
  }

  private def toFloat(s : Short) = s.toChar.toFloat
}
