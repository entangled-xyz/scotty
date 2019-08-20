package scotty.simulator.math

import scotty.quantum.QuantumContext.{Matrix, Vector}
import scotty.quantum.math.Complex

/**
  * The <code>Matrix</code> class contains a representation of a complex number square matrix.
  * Complex numbers are represented with two consecutive <code>Double</code>s. All matrix
  * data is represented in a single array.
  *
  * @param data Raw data representing all complex numbers in a flat array of <code>Double</code>s.
  */
case class MatrixWrapper(data: Matrix) {
  def dimension: Int = MatrixWrapper.dimension(data)
}

object MatrixWrapper {
  def dimension(data: Matrix): Int = data.length

  def apply(data: Array[Array[Complex]]): MatrixWrapper =
    MatrixWrapper(data.map(row => row.map(v => Array(v.r, v.i)).flatten))

  def identity(dimension: Int): Matrix = {
    val data = Array.fill(dimension)(Array.fill(dimension * 2)(0d))

    for (i <- 0 until dimension) {
      data(i * 2)(i * 2) = 1d
    }

    data
  }

  def product(matrix: Matrix, vector: Vector): Vector = {
    val newVector = Array.fill(vector.length)(0d)

    for (rowIndex <- matrix.indices) {
      val rowValue = Array(0d, 0d)

      for (index <- matrix.indices) {
        val product = Complex.product(
          matrix(rowIndex)(2 * index),
          matrix(rowIndex)(2 * index + 1),
          vector(2 * rowIndex),
          vector(2 * rowIndex + 1))

        rowValue(2 * rowIndex) = product._1
        rowValue(2 * rowIndex + 1) = product._2
      }

      newVector(rowIndex) = rowValue(0)
      newVector(rowIndex + 1) = rowValue(1)
    }

    newVector
  }

  def conjugateTranspose(data: Matrix): Matrix = {
    data
  }
}