package scotty.simulator.math

import scotty.quantum.math.Complex

/**
  * The <code>Matrix</code> class contains a representation of a complex number square matrix.
  * Complex numbers are represented with two consecutive <code>Double</code>s. All matrix
  * data is represented in a single array.
  *
  * @param data Raw data representing all complex numbers in a flat array of <code>Double</code>s.
  */
case class Matrix(data: Array[Double]) {
  def dimension: Int = Matrix.dimension(data)

  def tensorProduct(matrix: Matrix): Matrix = Matrix(Matrix.tensorProduct(data, matrix.data))
}

object Matrix {
  def dimension(data: Array[Double]): Int = Math.sqrt(data.length / 2).toInt

  def apply(data: Array[Array[Complex]]): Matrix = Matrix(data.flatten.map(c => Array(c.r, c.i)).flatten)

  def tensorProduct(m1: Array[Double], m2: Array[Double]): Array[Double] = {
    val thisDimension = m1.length
    val thatDimension = m2.length
    val newDimension = thisDimension * thatDimension
    val newData = Array.fill(newDimension * newDimension * 2)(0d)

    for (r1 <- 0 until thisDimension) {
      for (r2 <- 0 until thatDimension) {
        for (c1 <- 0 until thisDimension) {
          for (c2 <- 0 until thatDimension) {
            val k1 = (r1 * thisDimension + c1) * 2
            val k2 = (r2 * thatDimension + c2) * 2
            val k3 = ((r1 * thatDimension + r2) * newDimension + (c1 * thatDimension + c2)) * 2
            val (r, i) = Complex.product(m1(k1), m1(k1 + 1), m2(k2), m2(k2 + 1))

            newData(k3) = r
            newData(k3 + 1) = i
          }
        }
      }
    }

    newData
  }
}