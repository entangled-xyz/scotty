package scotty.simulator.math

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Gate, Matrix, Target}
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class RawGate(rawMatrix: Matrix) extends Target with MatrixTransformations {
  // Exact qubit indexes don't matter in this context because we only use this gate
  // for arbitrary matrix calculations.
  val index = -1

  override lazy val qubitCount = Math.sqrt(rawMatrix.length).toInt

  override def matrix()(implicit ctx: QuantumContext) = rawMatrix
}

object RawGate {
  def apply(gate: Gate)(implicit ctx: QuantumContext): RawGate = this(gate.matrix)
}