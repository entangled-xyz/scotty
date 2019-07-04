package scotty.simulator

import scotty.Labeled
import scotty.quantum.QuantumContext.Vector
import scotty.quantum._
import scotty.quantum.math.Complex
import scala.util.Random

case class SimSuperposition(qubitRegister: QubitRegister, vector: Vector, label: Option[String])
                           (implicit r: Random) extends Superposition with Labeled[String] {

  def combine(state: Superposition)(implicit ctx: QuantumContext): Superposition = {
    if (vector.length == 0) SimSuperposition(state)
    else ctx.tensorProduct(this, SimSuperposition(state))
  }

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition =  ctx.product(gate, this)

  def measure: Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val result = probabilities.foldLeft(initialIterator)((iterator, prob) => {
      val probSum = iterator._2 + prob
      val tryCollapse = (c: Int) => if (prob > 0 && r.nextDouble() <= probSum) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, probSum, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, probSum, valueOp)
      }
    })._3

    Collapsed(qubitRegister, result.get)
  }

  def withRegister(register: QubitRegister): SimSuperposition = SimSuperposition(register, vector, label)
}

object SimSuperposition {
  def apply()(implicit random: Random): SimSuperposition = this(Array[Complex]())

  def apply(q: Qubit)(implicit random: Random): SimSuperposition = this(Array(q.a, q.b))

  def apply(a: Complex, b: Complex)(implicit random: Random): SimSuperposition = this(Array(a, b))

  def apply(state: Superposition)(implicit random: Random): SimSuperposition = this(state.vector)

  // `SimSuperposition` is only used for vector computations, so we don't need a register reference here
  def apply(vector: Vector)(implicit random: Random): SimSuperposition = this(null, vector, None)

  def apply(vector: Vector, label: Option[String])(implicit random: Random): SimSuperposition = this(null, vector, label)
}