package scotty.simulator.math

import scotty.quantum.QuantumContext.{Matrix, Vector}
import scotty.quantum.math.Complex
import scotty.quantum.math.MathUtils._

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
  def dimension(matrix: Matrix): Int = matrix.length

  def apply(data: Array[Array[Complex]]): MatrixWrapper =
    MatrixWrapper(data.map(row => row.map(v => Array(v.r, v.i)).flatten))

  def isUnitary(matrix: Matrix): Boolean = areEqual(
    product(conjugateTranspose(matrix.map(r => r.clone)), matrix),
    identity(matrix.length)
  )

  def areEqual (m1: Matrix, m2: Matrix): Boolean = {
    // require square
    // require same dimension

    for (rowIndex <- m1.indices) {
      for (valueIndex <- m1.indices) {
        if ((m1(rowIndex)(2 * valueIndex) !~= m2(rowIndex)(2 * valueIndex)) ||
          (m1(rowIndex)(2 * valueIndex + 1) !~= m2(rowIndex)(2 * valueIndex + 1))) return false
      }
    }

    true
  }

  def identity(dimension: Int): Matrix = {
    val data = Array.fill(dimension)(Array.fill(dimension * 2)(0d))

    for (i <- 0 until dimension) {
      data(i)(i * 2) = 1d
    }

    data
  }

  def product(m1: Matrix, m2: Matrix): Matrix = {
    // require square
    // require same dimension

    val result = Array.fill(m1.length)(Array.fill(m1.length * 2)(0d))
    val dimensions = m1.indices

    for (rowIndex <- dimensions) {
      for (i <- dimensions) {
        var sumR = 0d
        var sumI = 0d

        for (j <- dimensions) {
          val r1 = m1(rowIndex)(2 * j)
          val i1 = m1(rowIndex)(2 * j + 1)
          val r2 = m2(j)(2 * i)
          val i2 = m2(j)(2 * i + 1)

          val product = Complex.product(r1, i1, r2, i2)

          sumR += product._1
          sumI += product._2
        }

        result(rowIndex)(2 * i) = sumR
        result(rowIndex)(2 * i + 1) = sumI
      }
    }

    result
  }

  def product(matrix: Matrix, vector: Vector): Vector = {
    // require square
    // require same dimension

    val newVector = Array.fill(vector.length)(0d)

    for (rowIndex <- matrix.indices) {
      val rowValue = Array(0d, 0d)

      for (valueIndex <- matrix.indices) {
        val doubleValueIndex = 2 * valueIndex

        val (r, i) = Complex.product(
          matrix(rowIndex)(doubleValueIndex),
          matrix(rowIndex)(doubleValueIndex + 1),
          vector(doubleValueIndex),
          vector(doubleValueIndex + 1))

        rowValue(0) += r
        rowValue(1) += i
      }

      newVector(2 * rowIndex) = rowValue(0)
      newVector(2 * rowIndex + 1) = rowValue(1)
    }

    newVector
  }

  def conjugateTranspose(matrix: Matrix): Matrix = {
    // require square

    val halfDimension = matrix.length / 2

    for (rowIndex <- matrix.indices) {
      for (valueIndex <- matrix.indices) {
        if (rowIndex == valueIndex) {
          matrix(rowIndex)(2 * valueIndex + 1) = -matrix(rowIndex)(2 * valueIndex + 1)
        } else if (rowIndex < halfDimension) {
          val doubleValueIndex = 2 * valueIndex
          val doubleRowIndex = 2 * rowIndex

          val r1 = matrix(rowIndex)(doubleValueIndex)
          val i1 = matrix(rowIndex)(doubleValueIndex + 1)

          val r2 = matrix(valueIndex)(doubleRowIndex)
          val i2 = matrix(valueIndex)(doubleRowIndex + 1)

          matrix(rowIndex)(doubleValueIndex) = r2
          matrix(rowIndex)(doubleValueIndex + 1) = -i2

          matrix(valueIndex)(doubleRowIndex) = r1
          matrix(valueIndex)(doubleRowIndex + 1) = -i1
        }
      }
    }

    matrix
  }
}