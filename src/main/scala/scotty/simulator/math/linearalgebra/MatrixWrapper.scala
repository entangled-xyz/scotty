package scotty.simulator.math.linearalgebra

import org.apache.commons.math3.complex.{Complex, ComplexField}
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, MatrixUtils}
import scotty.quantum.QuantumContext.Matrix
import Types.{ApacheMatrix, ApacheVector}
import scotty.quantum.math.MathUtils._

case class MatrixWrapper(matrix: Array[Array[Complex]]) {
  lazy val fieldMatrix: ApacheMatrix = MatrixWrapper.fieldMatrix(matrix)
  lazy val rowCount: Int = fieldMatrix.getRowDimension
  lazy val columnCount: Int = fieldMatrix.getColumnDimension

  def map(m: ApacheMatrix, f: Complex => Complex): ApacheMatrix = {
    val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, m.getRowDimension, m.getColumnDimension)

    for (rowIndex <- 0 until m.getRowDimension) {
      for (columnIndex <- 0 until m.getColumnDimension) {
        resultMatrix.setEntry(rowIndex, columnIndex, f(m.getEntry(rowIndex, columnIndex)))
      }
    }

    resultMatrix
  }

  def *(m: ApacheMatrix): ApacheMatrix = product(m)

  def *(v: ApacheVector): ApacheVector = product(v)

  def *(factor: Complex): ApacheMatrix = scalarProduct(factor)

  def ⊗(m: ApacheMatrix): ApacheMatrix = tensorProduct(m)

  def T: ApacheMatrix = conjugateTranspose

  def isUnitaryMatrix: Boolean = equals(roundValues(product(T, fieldMatrix)), identity)

  def roundValues(m: ApacheMatrix): ApacheMatrix =
    map(m, entry => new Complex(entry.getReal.rounded, entry.getImaginary.rounded))

  def conjugateTranspose: ApacheMatrix =
    new ApacheMatrix(fieldMatrix.transpose().getData.map(c => c.map(v => v.conjugate())), false)

  def identity: ApacheMatrix =
    new ApacheMatrix(MatrixUtils.createFieldIdentityMatrix(ComplexField.getInstance, rowCount).getData, false)

  def product(m: ApacheMatrix): ApacheMatrix = product(fieldMatrix, m)

  def product(m1: ApacheMatrix, m2: ApacheMatrix): ApacheMatrix = m1.multiply(m2)

  def ==(m: ApacheMatrix): Boolean = equals(m)

  def equals(m: ApacheMatrix): Boolean = fieldMatrix.equals(m)

  def scalarProduct(factor: Complex): ApacheMatrix = map(fieldMatrix, entry => entry.multiply(factor))

  def product(q: (Complex, Complex)): ApacheVector = product(Array(q._1, q._2))

  def product(vs: Array[Complex]): ApacheVector = product(
    new ArrayFieldVector[Complex](vs, false)
  )

  def product(v: ApacheVector): ApacheVector = {
    val resultVector = new ApacheVector(ComplexField.getInstance, v.getDimension)

    for (rowIndex <- 0 until rowCount) {
      var sum = new Complex(0)

      for (columnIndex <- 0 until columnCount) {
        sum = sum.add(fieldMatrix.getEntry(rowIndex, columnIndex).multiply(v.getEntry(columnIndex)))
      }

      resultVector.setEntry(rowIndex, sum)
    }

    resultVector
  }

  def tensorProduct(m: ApacheMatrix): ApacheMatrix = {
    val mRowCount = m.getRowDimension
    val mColumnCount = m.getColumnDimension
    val newRowCount = rowCount * mRowCount
    val newColumnCount = columnCount * mColumnCount
    val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, newRowCount, newColumnCount)

    for (rowIndex <- 0 until newRowCount) {
      for (columnIndex <- 0 until newColumnCount) {
        resultMatrix.setEntry(
          rowIndex,
          columnIndex,
          fieldMatrix.getEntry(rowIndex / mRowCount, columnIndex / mColumnCount).multiply(
            m.getEntry(rowIndex % mRowCount, columnIndex % mColumnCount)))
      }
    }

    resultMatrix
  }
}

object MatrixWrapper {
  def fieldMatrix(matrix: Matrix): ApacheMatrix = new Array2DRowFieldMatrix[Complex](matrix, false)
}