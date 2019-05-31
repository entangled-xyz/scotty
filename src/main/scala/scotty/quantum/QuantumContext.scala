package scotty.quantum

import scotty.quantum.math.Complex
import scotty.quantum.QuantumContext._

trait QuantumContext {
  def run(circuit: Circuit): State

  def controlMatrix(gate: Control): Matrix

  def par(gate1: Gate, gate2: Gate): Matrix

  def isUnitary(gate: Gate): Boolean

  def matrix(targetGate: Target): Matrix

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => s.measure()
      case s: Collapsed => s
    }
  }
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class Qubit(a: Complex, b: Complex)

  object Qubit {
    def one: Qubit = Qubit(Complex(0), Complex(1))

    def zero: Qubit = Qubit(Complex(1), Complex(0))
  }

  sealed trait Register[T] {
    val values: Seq[T]

    def size = values.length
  }

  case class QuantumRegister(values: Qubit*) extends Register[Qubit]

  case class BinaryRegister(values: Int*) extends Register[Int]
}