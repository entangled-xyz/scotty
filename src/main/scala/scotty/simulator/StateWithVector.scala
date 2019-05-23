package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Complex, Gate, Op, State}
import scotty.simulator.math.LinearAlgebra.VectorTransformations
import scotty.simulator.math.Implicits._

case class StateWithVector(rawVector: Array[Complex])
                          (implicit val computer: QuantumContext) extends State with VectorTransformations {
  def vector() = rawVector

  def combine(state: State): StateWithVector = {
    if (rawVector.length == 0) StateWithVector(state.vector())
    else StateWithVector((this ⊗ StateWithVector(state).fieldVector).getData)
  }

  def applyGate(gate: Gate): State = StateWithVector(GateWithMatrix(gate).product(fieldVector).getData)
}

object StateWithVector {
  def apply()(implicit ctx: QuantumContext): StateWithVector = this(Array[Complex]())
  def apply(base: (Complex, Complex))(implicit ctx: QuantumContext): StateWithVector = this(Array(base._1, base._2))
  def apply(state: State)(implicit ctx: QuantumContext): StateWithVector = this(state.vector())
}