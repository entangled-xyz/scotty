package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.QuantumContext._

trait QuantumContext {
  def run(circuit: Circuit): State

  def controlMatrix(gate: Control): Matrix

  def swapMatrix(gate: QubitSwap): Matrix

  def par(gate1: Gate, gate2: Gate): Matrix

  def isUnitary(gate: Gate): Boolean

  def targetMatrix(target: Target): Matrix

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => s.measure
      case s: Collapsed => s
    }
  }
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class Qubit(a: Complex, b: Complex) {
    require(Qubit.areAmplitudesValid(this), ErrorMessage.QubitAmplitudesError)
  }

  object Qubit {
    def one: Qubit = Qubit(Complex(0), Complex(1))

    def zero: Qubit = Qubit(Complex(1), Complex(0))

    def fiftyFifty: Qubit = this(Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)))

    def areAmplitudesValid(q: Qubit): Boolean = MathUtils.isProbabilityValid(q.a.abs, q.b.abs)

    def apply(as: Array[Complex]): Qubit = this(as(0), as(1))
  }

  sealed trait Register[T] {
    val values: Seq[T]

    def size: Int = values.length
  }

  case class QuantumRegister(values: Seq[Qubit]) extends Register[Qubit]

  case class BinaryRegister(values: Seq[Int]) extends Register[Int]
}