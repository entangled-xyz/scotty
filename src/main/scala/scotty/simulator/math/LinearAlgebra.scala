package scotty.simulator.math

import org.apache.commons.math3.complex.{ComplexField, Complex => ApacheComplex}
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, MatrixUtils}
import scotty.quantum.math.MathUtils._
import scotty.quantum.math.Complex
import scotty.simulator.math.Implicits._

object LinearAlgebra {
  type Matrix = Array2DRowFieldMatrix[ApacheComplex]
  type Vector = ArrayFieldVector[ApacheComplex]

  trait MatrixTransformations {
    val rawMatrix: Array[Array[Complex]]

    lazy val fieldMatrix = new Array2DRowFieldMatrix[ApacheComplex](rawMatrix, false)
    lazy val rowCount: Int = fieldMatrix.getRowDimension
    lazy val columnCount: Int = fieldMatrix.getColumnDimension

    def map(m: Matrix, f: ApacheComplex => ApacheComplex): Matrix = {
      val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, m.getRowDimension, m.getColumnDimension)

      for (rowIndex <- 0 until m.getRowDimension) {
        for (columnIndex <- 0 until m.getColumnDimension) {
          resultMatrix.setEntry(rowIndex, columnIndex, f(m.getEntry(rowIndex, columnIndex)))
        }
      }

      resultMatrix
    }

    def *(m: Matrix): Matrix = product(m)

    def *(v: Vector): Vector = product(v)

    def *(factor: Complex): Matrix = scalarProduct(factor)

    def ⊗(m: Matrix): Matrix = kroneckerProduct(m)

    def T: Matrix = conjugateTranspose

    def isUnitaryMatrix: Boolean = equals(round(product(T, fieldMatrix)), identity)

    def round(m: Matrix): Matrix =
      map(m, entry => Complex(entry.getReal.roundWithPrecision, entry.getImaginary.roundWithPrecision))

    def round: Matrix = round(fieldMatrix)

    def conjugateTranspose: Matrix = new Matrix(fieldMatrix.transpose().getData.map(c => c.map(v => v.conjugate())), false)

    def identity: Matrix =
      new Matrix(MatrixUtils.createFieldIdentityMatrix(ComplexField.getInstance, rowCount).getData, false)

    def product(m: Matrix): Matrix = product(fieldMatrix, m)

    def product(m1: Matrix, m2: Matrix): Matrix = m1.multiply(m2)

    def ==(m: Matrix): Boolean = equals(m)

    def equals(m: Matrix): Boolean = equals(fieldMatrix, m)

    def equals(m1: Matrix, m2: Matrix): Boolean = m1.equals(m2)

    def scalarProduct(factor: Complex): Matrix = map(fieldMatrix, entry => entry.multiply(factor))

    def product(q: (Complex, Complex)): Vector = product(Array(q._1, q._2))

    def product(vs: Array[Complex]): Vector = product(
      new ArrayFieldVector[ApacheComplex](vs, false)
    )

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

    def ⊗(v: Vector): Vector = kroneckerProduct(v)

    def *(factor: Complex): Vector = scalarProduct(factor)

    def scalarProduct(factor: Complex): Vector = map(entry => entry.multiply(factor))

    def kroneckerProduct(v: Vector): Vector = {
      val v1Size = fieldVector.getDimension
      val v2Size = v.getDimension
      val resultVector = new ArrayFieldVector(ComplexField.getInstance, v1Size * v2Size)

      for (v1Index <- 0 until v1Size) {
        for (v2Index <- 0 until v2Size) {
          resultVector.setEntry(
            (v1Index * v2Size) + v2Index,
            fieldVector.getEntry(v1Index).multiply(v.getEntry(v2Index))
          )
        }
      }

      resultVector
    }
  }
}