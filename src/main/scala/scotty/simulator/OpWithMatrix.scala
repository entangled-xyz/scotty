package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Complex, Op, Qubit}
import scotty.simulator.math.Implicits._
import scotty.simulator.math.LinearAlgebra.MatrixTransformations

case class OpWithMatrix(rawMatrix: Array[Array[Complex]])
                       (implicit val computer: QuantumContext) extends Op with MatrixTransformations {
  val qs: Seq[Qubit] = Seq() // qubits don't matter in this context

  override lazy val matrixGenerator = () => rawMatrix

  override def combine(op: Op): OpWithMatrix = OpWithMatrix((this ⊗ OpWithMatrix(op).fieldMatrix).getData)
}

object OpWithMatrix {
  def apply(op: Op)(implicit ctx: QuantumContext): OpWithMatrix = this(op.matrixGenerator.apply())
}