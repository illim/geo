package priv.util

import java.nio.{ByteBuffer,IntBuffer,FloatBuffer}
import java.nio.ByteOrder

object BufferUtils {
  val floatSize = 4

  final def createFloatBuffer(size : Int) : FloatBuffer = {
    createByteBuffer(size * floatSize).asFloatBuffer()
  }

  final def createByteBuffer(size : Int) : ByteBuffer = {
    val b = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
    b.clear()
    b
  }

}
