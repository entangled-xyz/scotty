package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Complex, Gate, Superposition, Vector}
import scotty.simulator.math.RawGate
import scotty.simulator.math.LinearAlgebra.VectorTransformations
import scotty.simulator.math.Implicits._

case class SimSuperposition(vector: Vector)
                           (implicit val computer: QuantumContext) extends Superposition with VectorTransformations {
  val rawVector = vector

  def parCombination(state: Superposition): SimSuperposition = {
    if (rawVector.length == 0) SimSuperposition(state.vector)
    else SimSuperposition((this ⊗ SimSuperposition(state).fieldVector).getData)
  }

  def applyGate(gate: Gate): Superposition = SimSuperposition(RawGate(gate).product(fieldVector).getData)
}

object SimSuperposition {
  def apply()(implicit ctx: QuantumContext): SimSuperposition = this(Array[Complex]())
  def apply(base: (Complex, Complex))(implicit ctx: QuantumContext): SimSuperposition = this(Array(base._1, base._2))
  def apply(state: Superposition)(implicit ctx: QuantumContext): SimSuperposition = this(state.vector)
}