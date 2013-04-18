package priv

import java.io._
import javax.imageio.ImageIO
import javax.imageio.stream._
import org.newdawn.slick.util.ResourceLoader
import Common._

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

// 16 bit little endian
class RawSource(name : String) extends QuadSource {
  private val is = ResourceLoader.getResourceAsStream(name)
  val (array, size) = {
    val bout = new ByteArrayOutputStream()
    var n = is.available()
    while(n > 0){
      bout.write(is.read())
      n = is.available()
    }
    val ba = bout.toByteArray
    val count = ba.length / 2
    val res = new Array[Short](count)
    println(ba.take(20).toList)
    for(i <- 0 until count) {
      res(i) = ((ba(1 + (2 * i)) << 8) + (ba(2 * i) & 0xff)).toShort
    }
    (res, math.sqrt(count).toInt - 1)
  }

  val minMax = {
    val first = array(0).toFloat
    ((first, first) /: array){ case ((min, max), c) =>
      val elt = c.toFloat
      (if (elt < min) elt else min, if (elt > max) elt else max)
    }
  }

  def read(x : Int, y : Int, blockSize : Int) = {
    val res = new Array[Float](pow2(blockSize + 1))
    for( i <- 0 to blockSize; j <- 0 to blockSize){
      res(i + j * (blockSize + 1)) = array(x + i + (y + j) *  (size + 1)).toFloat
    }
    res
  }

}
