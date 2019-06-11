package scotty.simulator.math

import scotty.quantum.{Gate, QuantumContext, Target}
import scotty.quantum.QuantumContext.Matrix
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class RawGate(rawMatrix: Matrix) extends Target with MatrixTransformations {
  // Exact qubit indexes don't matter in this context because we only use this gate
  // for arbitrary matrix calculations.
  val index1 = -1

  override lazy val qubitCount = Math.sqrt(rawMatrix.length).toInt

  override def targetMatrix = Some(rawMatrix)
}

object RawGate {
  def apply(gate: Gate)(implicit ctx: QuantumContext): RawGate = this(gate.matrix)
}