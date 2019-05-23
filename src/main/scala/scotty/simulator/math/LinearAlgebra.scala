package scotty.simulator.math

import org.apache.commons.math3.complex.{ComplexField, Complex => ApacheComplex}
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, MatrixUtils}
import scotty.quantum.QuantumComputer.Complex
import scotty.simulator.math.Implicits._

object LinearAlgebra {
  type Matrix = Array2DRowFieldMatrix[ApacheComplex]
  type Vector = ArrayFieldVector[ApacheComplex]

  trait MatrixTransformations {
    val rawMatrix: Array[Array[Complex]]

    lazy val fieldMatrix = new Array2DRowFieldMatrix[ApacheComplex](rawMatrix, false)
    lazy val rowCount: Int = fieldMatrix.getRowDimension
    lazy val columnCount: Int = fieldMatrix.getColumnDimension

    def toMatrix(array: Array[Array[Complex]]) = new Matrix(array, false)

    def toVector(array: Array[Complex]) = new Vector(array, false)

    def map(f: ApacheComplex => ApacheComplex): Matrix = {
      val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, rowCount, columnCount)

      for (rowIndex <- 0 until rowCount) {
        for (columnIndex <- 0 until columnCount) {
          resultMatrix.setEntry(rowIndex, columnIndex, f(fieldMatrix.getEntry(rowIndex, columnIndex)))
        }
      }

      resultMatrix
    }

    def *(v: Matrix): Matrix = product(v)

    def *(v: Vector): Vector = product(v)

    def *(factor: Complex): Matrix = scalarProduct(factor)

    def ⋅(v: Vector): Vector = product(v)

    def ⊗(v: Matrix): Matrix = kroneckerProduct(v)

    def T: Matrix = conjugateTranspose

    def round: Matrix = map(entry => Complex(MathUtils.round(entry.getReal), MathUtils.round(entry.getImaginary)))

    def conjugateTranspose: Matrix = new Matrix(fieldMatrix.transpose().getData.map(c => c.map(v => v.conjugate())), false)

    def identity: Matrix =
      new Matrix(MatrixUtils.createFieldIdentityMatrix(ComplexField.getInstance, rowCount).getData, false)

    def product(m: Matrix): Matrix = fieldMatrix.multiply(m)

    def ==(m: Matrix): Boolean = fieldMatrix.equals(m)

    def scalarProduct(factor: Complex): Matrix = map(entry => entry.multiply(factor))

    def product(v: Vector): Vector = {
      val resultVector = new Vector(ComplexField.getInstance, v.getDimension)

      for (rowIndex <- 0 until rowCount) {
        var sum = Complex(0)

        for (columnIndex <- 0 until columnCount) {
          sum = sum.add(fieldMatrix.getEntry(rowIndex, columnIndex).multiply(v.getEntry(columnIndex)))
        }

        resultVector.setEntry(rowIndex, sum)
      }

      resultVector
    }

    def kroneckerProduct(m: Matrix): Matrix = {
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

  trait VectorTransformations {
    val rawVector: Array[Complex]

    lazy val fieldVector = new ArrayFieldVector[ApacheComplex](rawVector, false)

    def map(f: ApacheComplex => ApacheComplex): Vector = {
      val resultVector = new ArrayFieldVector(ComplexField.getInstance, fieldVector.getDimension)

      for (index <- 0 until fieldVector.getDimension) {
        resultVector.setEntry(index, f(fieldVector.getEntry(index)))
      }

      resultVector
    }

    def ⊗(state: Vector): Vector = kroneckerProduct(state)

    def *(factor: Complex): Vector = scalarProduct(factor)

    def scalarProduct(factor: Complex): Vector = map(entry => entry.multiply(factor))

    def kroneckerProduct(state: Vector): Vector = {
      val v1Size = fieldVector.getDimension
      val v2Size = state.getDimension
      val resultVector = new ArrayFieldVector(ComplexField.getInstance, v1Size * v2Size)

      for (v1Index <- 0 until v1Size) {
        for (v2Index <- 0 until v2Size) {
          resultVector.setEntry(
            (v1Index * v2Size) + v2Index,
            fieldVector.getEntry(v1Index).multiply(state.getEntry(v2Index))
          )
        }
      }

      resultVector
    }
  }
}