package priv

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio._
import java.awt.image.BufferedImage

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

case class QuadScale(
  hfun : Float => Float = identity,
  xfactor : Int = 1)

case class QuadTree(root : Node, blocks : Array[Array[Node]], blockSize : Int, depth : Int)

object Node {
  val lt = 0  // top left corner : xmin, zmin
  val rt = 1
  val lb = 2
  val rb = 3
}

import Node._

sealed trait Node{
  def corners : List[Vec3]
  lazy val center = corners(lb) + ((corners(rt) - corners(lb)) * 0.5f)
  lazy val lbCorner = corners(lb)
  var visible = false
  def setVisible(frustum : Frustum)
  final def isVisible(frustum : Frustum) = {
    val cnt = corners.count(frustum.isVisible _)
    if (cnt > 0) cnt else {
      val pc = frustum.project(center)
      val plb = frustum.project(lbCorner)
      val diagSize = (pc - plb).size * 2
      val d = frustum.dist(pc)
      if (d < diagSize) 1 else 0 // bs
    }
  }
  def makeVisible(b : Boolean)
  def render()
}

class Patch(x : Int, y : Int, val size : Int, val children : Array[Node]) extends Node{
  val corners = List(
    children(0).corners(lt),
    children(1).corners(rt),
    children(2).corners(lb),
    children(3).corners(rb))

  def setVisible(frustum : Frustum){
    val nbVisibles = isVisible(frustum)
    if (nbVisibles > 0) {
      visible = true
      if (nbVisibles == 2) {
        makeVisible(true)
      } else {
        children.foreach(_.setVisible(frustum))
      }
    } else {
      visible = false
      makeVisible(false)
    }
  }
  def makeVisible(b : Boolean) = {
    visible = b
    children.foreach(_.makeVisible(b))
  }
  def render(){
    if (visible){
      children.foreach(_.render())
    }
  }
  override def toString() = x+"/"+y+"("+lt + "/" + rt + "/" + lb + "/" + rb+")"
}

class QuadBuilder(blockSize : Int = 4, scale : QuadScale = QuadScale()) {

  def build(source : QuadSource) = {
    val size = source.size -1

    // create the terrain blocks
    val nbBlock = size / blockSize // nb of blocks on 1 axis
    val blockRange = 0 until nbBlock
    val blocks = Array.ofDim[Node](nbBlock, nbBlock)

    for{
      j <- blockRange
      i <- blockRange
    }{
      val x = i * blockSize
      val y = j * blockSize
      val heights = source.read(x, y, blockSize)
      val sizedHeights = heights.map(scale.hfun)
      blocks(j)(i) = new TerrainBlock(x, y, blockSize, sizedHeights)
    }

    // create the patchs
    var childPatchSize = blockSize
    var patchSize = blockSize * 2
    var children = blocks
    var depth = 1
    while (patchSize <= size) {
      val nbPatch = size / patchSize
      val pachRange = 0 until nbPatch
      val patchs = Array.ofDim[Node](nbPatch, nbPatch)

      for {
        j <- pachRange
        i <- pachRange
      } {
        val x = i * patchSize
        val y = j * patchSize
        val xc = x / childPatchSize
        val yc = y / childPatchSize

        patchs(j)(i) = new Patch( x, y, patchSize,
                                  Array(
                                    children(yc)(xc),
                                    children(yc)(xc +1),
                                    children(yc+1)(xc),
                                    children(yc+1)(xc +1)))
      }
      childPatchSize = patchSize
      children = patchs
      patchSize = patchSize * 2
      depth += 1
    }
    assert(children.length == 1, children.length)
    QuadTree(children(0)(0), blocks, blockSize, depth)
  }


  lazy val (vbo, buff) = {
    val buff = util.BufferUtils.createFloatBuffer(blockSize * blockSize * 3 * 5)
    val vbo = glGenBuffers()
    (vbo, buff)
  }

  class TerrainBlock(val x : Int, val y : Int, val size : Int, val data : Array[Float] ) extends Node {
    val corners = List(vec(0, 0), vec(size, 0), vec(0, size), vec(size, size))

    def setVisible(frustum : Frustum) {
      visible = (isVisible(frustum) > 0)
    }

    def makeVisible(b : Boolean){
      visible = b
    }

    def render(){
      if (visible){
        glEnableVertexAttribArray(0)
        glBindBuffer(GL_ARRAY_BUFFER, vbo)
        val vCount = setData()
        glBufferData(GL_ARRAY_BUFFER, buff, GL_STATIC_DRAW)
        glVertexAttribPointer(0, 3, GL_FLOAT, false, 0, 0)
        glDrawArrays(GL_TRIANGLE_STRIP, 0, vCount)
        glDisableVertexAttribArray(0)
      }
    }

    private def setData() = {
      buff.rewind()
      var vCount = 0
      def put(i : Int, j : Int){
        val v = vec(i, j)
        buff.put(v.x)
        buff.put(v.y)
        buff.put(v.z)
        vCount += 1
      }
      var j = 0
      while (j < size) {
        if (j % 2 == 0){
          var i = 0
          while (i < size){
            put(i, j)
            put(i, j+1)
            put(i+1, j+1)
            put(i, j)
            put(i+1, j)
            i+=1
          }
        } else {
          var i = size
          while (i > 0){
            put(i, j)
            put(i, j+1)
            put(i-1, j+1)
            put(i, j)
            put(i-1, j)
            i-=1
          }
        }
        j+=1
      }
      buff.rewind()
      vCount
    }

    private def vec(i : Int, j : Int) = {
      val h = data(i + j * (size + 1))
      Vec3(scale.xfactor * (x+i), h, scale.xfactor * (y+j))
    }

    override def toString() = x+"/"+y+"("+ corners + "/" + data.toList+")"
  }

}
