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

object Node {
  val lt = 0  // top left corner : xmin, zmin
  val rt = 1
  val lb = 2
  val rb = 3

  val left = 0
  val right = 1
  val bottom = 2
  val top = 3
  type Blocks = Array[Array[Node]]
}

import Node._

case class QuadTree(root : Node, blocks : Blocks, blockSize : Int, depth : Int)


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
    val blocks = Array.ofDim[TerrainBlock](nbBlock, nbBlock)

    for( j <- blockRange ; i <- blockRange) {
      val x = i * blockSize
      val y = j * blockSize
      val heights = source.read(x, y, blockSize)
      val sizedHeights = heights.map(scale.hfun)
      blocks(j)(i) = new TerrainBlock(x, y, blockSize, sizedHeights)
    }

    for( j <- blockRange ; i <- blockRange) {
      blocks(j)(i).edges = List(
        if (i == 0) None else Some(blocks(j)(i - 1)),
        if (i == blocks(0).length - 1) None else Some(blocks(j)(i + 1)),
        if (j == blocks.length - 1) None else Some(blocks(j+1)(i)),
        if (j == 0) None else Some(blocks(j -1)(i)))
    }

    // create the patchs
    var childPatchSize = blockSize
    var patchSize = blockSize * 2
    var children = blocks.asInstanceOf[Blocks]
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
    QuadTree(children(0)(0), blocks.asInstanceOf[Blocks], blockSize, depth)
  }


  lazy val (vbo, buff) = {
    val buff = util.BufferUtils.createFloatBuffer(blockSize * blockSize * 3 * 5)
    val vbo = glGenBuffers()
    (vbo, buff)
  }

  class TerrainBlock(val x : Int, val y : Int, val size : Int, val data : Array[Float] ) extends Node {
    val corners = List(vec(0, 0), vec(size, 0), vec(0, size), vec(size, size))
    var edges = List.empty[Option[TerrainBlock]]
    var level = 2

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
        if (level == 1) {
          glDrawArrays(GL_TRIANGLE_STRIP, 0, vCount)
        } else {
          glDrawArrays(GL_TRIANGLES, 0, vCount)
        }
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
      if (level == 1){
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
      } else if (level > 1){
        val step = level
        val topEdgeSum = y * 2 + y + step
        val bottomEdgeSum = (y + size) * 2 + y + size - step
        val leftEdgeSum = x * 2 + x + step
        val rightEdgeSum = (x + size) * 2 + x + size - step
        val smooths = edges.map(_.exists(_.level < level))
        def addTriangle(i : Int, j : Int, k: Int, l : Int, m : Int, n : Int) = {
          val ysum = j + l + n
          val xsum = i + k + m
          if ((smooths(top) && ysum == topEdgeSum)
              || (smooths(bottom) && ysum == bottomEdgeSum)) {
            assert(j == n)
            var ii = i
            var iii = i + 1
            while( iii <= m){
              put(ii, j)
              put(k, l)
              put(iii, j)
              ii = iii
              iii += 1
            }
          } else if ((smooths(left) && xsum == leftEdgeSum)
              || (smooths(right) && xsum == rightEdgeSum)) {
            assert(i == m)
            var jj = j
            var jjj = j + 1
            while( jjj <= n){
              put(i, jj)
              put(k, l)
              put(i, jjj)
              jj = jjj
              jjj += 1
            }
          } else {
            put(i, j)
            put(k, l)
            put(m, n)
          }
        }
        var j = 0
        while (j < size) {
          var i = 0
          while (i < size){
            addTriangle(i, j, i, j+step, i+step, j+step)
            addTriangle(i, j, i+step, j+step, i+step, j)
            i+=step
          }
          j+=step
        }
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
