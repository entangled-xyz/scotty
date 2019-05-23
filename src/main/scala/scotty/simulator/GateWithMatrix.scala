package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Complex, Gate, Op, Qubit}
import scotty.simulator.math.Implicits._
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class GateWithMatrix(rawMatrix: Array[Array[Complex]])
                         (implicit val computer: QuantumContext) extends Gate with MatrixTransformations {
  val qs: Seq[Qubit] = Seq() // qubits don't matter in this context

  override lazy val matrixGenerator = () => rawMatrix

  override def combine(gate: Gate): GateWithMatrix = GateWithMatrix((this ⊗ GateWithMatrix(gate).fieldMatrix).getData)
}

object GateWithMatrix {
  def apply(gate: Gate)(implicit ctx: QuantumContext): GateWithMatrix = this(gate.matrixGenerator.apply())
}