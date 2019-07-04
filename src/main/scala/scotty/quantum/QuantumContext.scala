package scotty.quantum

import scotty.quantum.math.Complex
import scotty.quantum.QuantumContext._

trait QuantumContext {
  def run(circuit: Circuit): State

  def controlMatrix(gate: Control): Matrix

  def swapMatrix(gate: QubitSwap): Matrix

  def targetMatrix(target: Gate): Matrix

  def tensorProduct(gate1: Gate, gate2: Gate): Gate

  def tensorProduct(sp1: Superposition, sp2: Superposition): Superposition

  def product(gate: Gate, sp: Superposition): Superposition

  def isUnitary(gate: Gate): Boolean

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
}