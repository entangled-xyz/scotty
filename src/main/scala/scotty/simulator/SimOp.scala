package scotty.simulator

import org.apache.commons.math3.complex.ComplexField
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, MatrixUtils}
import scotty.quantum.QuantumMachine.{Complex, Op}
import scotty.simulator.math.MathUtils
import scotty.simulator.QuantumSim.Implicits._
import org.apache.commons.math3.complex.{Complex => ApacheComplex}

case class SimOp(data: Array[Array[Complex]])(implicit val sim: QuantumSim) extends Op {
  lazy val qubitCount: Int = (Math.log10(rowCount) / Math.log10(2)).toInt
  lazy val isUnitary: Boolean = (T * this).round == identity

  lazy val fieldMatrix = new Array2DRowFieldMatrix[ApacheComplex](data, false)
  lazy val rowCount: Int = fieldMatrix.getRowDimension
  lazy val columnCount: Int = fieldMatrix.getColumnDimension

  def combine(op: Op): Op = this ⊗ SimOp(op.data)

  def map(f: ApacheComplex => ApacheComplex): SimOp = {
    val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, rowCount, columnCount)

    for (rowIndex <- 0 until rowCount) {
      for (columnIndex <- 0 until columnCount) {
        resultMatrix.setEntry(rowIndex, columnIndex, f(fieldMatrix.getEntry(rowIndex, columnIndex)))
      }
    }

    SimOp(resultMatrix.getData)
  }

  def *(v: SimOp): SimOp = product(v)

  def *(v: SimState): SimState = product(v)

  def ⋅(v: SimState): SimState = product(v)

  def ⊗(v: SimOp): SimOp = kroneckerProduct(v)

  def T: SimOp = conjugateTranspose

  def round: SimOp = map(entry => Complex(MathUtils.round(entry.getReal), MathUtils.round(entry.getImaginary)))

  def conjugateTranspose: SimOp = SimOp(fieldMatrix.transpose().getData.map(c => c.map(v => v.conjugate())))

  def identity: SimOp = SimOp(MatrixUtils.createFieldIdentityMatrix(ComplexField.getInstance, rowCount).getData)

  def product(m: SimOp): SimOp = SimOp(fieldMatrix.multiply(m.fieldMatrix).getData)

  def ==(m: SimOp): Boolean = fieldMatrix.equals(m.fieldMatrix)

  def scalarProduct(factor: Complex): SimOp = map(entry => entry.multiply(factor))

  def product(v: SimState): SimState = {
    val resultVector = new ArrayFieldVector(ComplexField.getInstance, v.fieldVector.getDimension)

    for (rowIndex <- 0 until rowCount) {
      var sum = Complex(0)

      for (columnIndex <- 0 until columnCount) {
        sum = sum.add(fieldMatrix.getEntry(rowIndex, columnIndex).multiply(v.fieldVector.getEntry(columnIndex)))
      }

      resultVector.setEntry(rowIndex, sum)
    }

    SimState(resultVector.getData)
  }

  def kroneckerProduct(m: SimOp): SimOp = {
    val mRowCount = m.rowCount
    val mColumnCount = m.columnCount
    val newRowCount = rowCount * mRowCount
    val newColumnCount = columnCount * mColumnCount
    val resultMatrix = new Array2DRowFieldMatrix(ComplexField.getInstance, newRowCount, newColumnCount)

    for (rowIndex <- 0 until newRowCount) {
      for (columnIndex <- 0 until newColumnCount) {
        resultMatrix.setEntry(
          rowIndex,
          columnIndex,
          fieldMatrix.getEntry(rowIndex / mRowCount, columnIndex / mColumnCount).multiply(
            m.fieldMatrix.getEntry(rowIndex % mRowCount, columnIndex % mColumnCount)))
      }
    }

    SimOp(resultMatrix.getData)
  }
}
