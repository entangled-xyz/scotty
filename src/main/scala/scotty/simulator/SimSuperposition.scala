package scotty.simulator

import scotty.quantum.QuantumContext.{Qubit, Vector}
import scotty.quantum._
import scotty.simulator.math.LinearAlgebra.VectorTransformations
import scotty.simulator.math.Implicits._
import scotty.simulator.math.RawGate
import scotty.quantum.math.Complex

import scala.util.Random

case class SimSuperposition(vector: Vector)
                           (implicit random: Random) extends Superposition with VectorTransformations {
  val rawVector = vector

  def par(state: Superposition): SimSuperposition = {
    if (rawVector.length == 0) SimSuperposition(state.vector)
    else SimSuperposition((this ⊗ SimSuperposition(state).fieldVector).getData)
  }

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition =
    SimSuperposition(RawGate(gate).product(fieldVector).getData)

  def measure(): Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val result = probabilities().foldLeft(initialIterator)((iterator, prob) => {
      val probSum = iterator._2 + prob
      val tryCollapse = (c: Int) => if (prob > 0 && random.nextDouble() <= probSum) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, probSum, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, probSum, valueOp)
      }
    })._3

    Collapsed(qubitCount, result.get)
  }
}

object SimSuperposition {
  def apply()(implicit random: Random): SimSuperposition = this(Array[Complex]())
  def apply(q: Qubit)(implicit random: Random): SimSuperposition = this(Array(q.a, q.b))
  def apply(a: Complex, b: Complex)(implicit random: Random): SimSuperposition = this(Array(a, b))
  def apply(state: Superposition)(implicit random: Random): SimSuperposition = this(state.vector)
}