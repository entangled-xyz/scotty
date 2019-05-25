package scotty.simulator.math

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Gate, Matrix, Qubit}
import scotty.simulator.math.Implicits._
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class RawGate(rawMatrix: Matrix)
                  (implicit val computer: QuantumContext) extends Gate with MatrixTransformations {
  val qs: Seq[Qubit] = Seq() // qubits don't matter in this context

  override lazy val matrix = rawMatrix

  override def combine(gate: Gate): RawGate = RawGate((this ⊗ RawGate(gate).fieldMatrix).getData)
}

object RawGate {
  def apply(gate: Gate)(implicit ctx: QuantumContext): RawGate = this(gate.matrix)
}