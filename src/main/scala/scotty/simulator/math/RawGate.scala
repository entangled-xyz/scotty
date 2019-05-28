package scotty.simulator.math

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Gate, Matrix, Qubit, Target}
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class RawGate(rawMatrix: Matrix) extends Target with MatrixTransformations {
  val qs: Seq[Qubit] = Seq() // qubits don't matter in this context

  override def matrix()(implicit ctx: QuantumContext) = rawMatrix
}

object RawGate {
  def apply(gate: Gate)(implicit ctx: QuantumContext): RawGate = this(gate.matrix)
}