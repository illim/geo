package priv

trait Example extends QuadSource {
  def elements : Array[Array[Float]]
  def read(x : Int, y : Int, blockSize : Int) : Array[Float] = {
    val res = Array.ofDim[Float]((blockSize + 1) * (blockSize + 1))
    for(j <- 0 to blockSize){
      for(i <- 0 to blockSize){
        val a = elements(y + j)(x + i)
        res(i + j * (blockSize + 1)) = a
      }
    }
    res
  }
}

object Example1 extends Example {
  val size = 3
  val elements = Array(
    Array[Float](1, 1, 1),
    Array[Float](1, 1, 1),
    Array[Float](1, 1, 1))
  val minMax = (1f, 1f)
}


object Example2 extends Example {
  val size = 5
  val elements = Array(
    Array[Float](0, 1, 3, 4, 5),
    Array[Float](6, 7, 8, 9, 10),
    Array[Float](11, 12, 13, 14, 15),
    Array[Float](21, 22, 23, 24, 25),
    Array[Float](31, 32, 33, 34, 35))
  val minMax = (0f, 35f)
}

