package scotty.quantum.gate

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.gate.Gate.GateGen

case class DefGate(matrixGen: GateGen, override val params: Seq[Double], indexes: Seq[Int]) extends TargetGate {
  val matrix: Matrix = matrixGen.apply(params)

  override val customMatrix: Option[Matrix] = Some(matrix)

  def indexesMatchMatrixDimensions: Boolean = {
    val lengthWithGaps =
      if (indexes.length <= 1) indexes.length
      else 1 + indexes.max - indexes.min

    Math.pow(2, lengthWithGaps) == matrix.length && matrix.forall(r => r.length == matrix.length)
  }

  require(indexesMatchMatrixDimensions, ErrorMessage.GateMatrixDoesntMatchIndexes)
}

object DefGate {
  def apply(matrix: Matrix, indexes: Int*): DefGate = this((_: Seq[Double]) => matrix, Seq(), indexes)

  def apply(matrixGen: GateGen, param: Double, indexes: Int*): DefGate =
    this(matrixGen, Seq(param), indexes)
}