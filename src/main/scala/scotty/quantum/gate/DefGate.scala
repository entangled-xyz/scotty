package scotty.quantum.gate

import scotty.ErrorMessage
import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.gate.Gate.GateGen

case class DefGate(matrixGen: GateGen, override val params: Seq[Double], index: Int)
                  (implicit ctx: QuantumContext) extends TargetGate {
  val matrix: Matrix = matrixGen.apply(params)

  override val customMatrix: Option[Matrix] = Some(matrix)

  def indexesMatchMatrixDimensions: Boolean = {
    val lengthWithGaps =
      if (indexes.length <= 1) indexes.length
      else 1 + indexes.max - indexes.min

    Math.pow(2, lengthWithGaps) == matrix.length && matrix.forall(r => r.length / 2 == matrix.length)
  }

  require(indexesMatchMatrixDimensions, ErrorMessage.GateMatrixDoesntMatchIndexes)
  require(ctx.isUnitary(this), ErrorMessage.GateMatrixNotUnitary)
}

object DefGate {
  def apply(matrix: Matrix, index: Int)
           (implicit ctx: QuantumContext): DefGate = this((_: Seq[Double]) => matrix, Seq(), index)

  def apply(matrixGen: GateGen, param: Double, index: Int)(implicit ctx: QuantumContext): DefGate =
    this(matrixGen, Seq(param), index)
}