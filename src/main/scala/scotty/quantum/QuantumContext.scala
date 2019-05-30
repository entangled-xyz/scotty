package scotty.quantum

import scotty.quantum.math.Complex
import scotty.quantum.QuantumContext._

trait QuantumContext {
  def run(circuit: Circuit): Superposition

  def controlMatrix(gate: Control): Matrix

  def par(gate1: Gate, gate2: Gate): Matrix

  def isUnitary(gate: Gate): Boolean

  def matrix(targetGate: Target): Matrix

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit).measure()
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
}