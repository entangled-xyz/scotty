package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.quantum.gate.Gate

trait QuantumContext {
  def run(circuit: Circuit): State

  def gateMatrix(gate: Gate): Matrix

  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition

  def product(register: QubitRegister, gate: Gate, sp: Superposition): Superposition

  def densityMatrix(vector: Vector): Matrix

  def isUnitary(gate: Gate): Boolean

  def measure(register: QubitRegister, state: Array[Double]): Collapsed

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => measure(circuit.register, s.vector)
      case s: Collapsed => s
    }
  }

  def runAndMeasure(circuit: Circuit, trialsCount: Int): ExperimentResult
}

object QuantumContext {
  type Vector = Array[Double]
  type Matrix = Array[Array[Double]]

  case class QuantumException(message: String) extends Exception(message)
}