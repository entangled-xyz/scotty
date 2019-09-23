package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.gate.Gate

trait QuantumContext {
  def gateMatrix(gate: Gate): Matrix

  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition

  def product(register: QubitRegister, gate: Gate, sp: Superposition): Superposition

  def densityMatrix(vector: Vector): Matrix

  def isUnitary(gate: Gate): Boolean

  def superpositionProbabilities(sp: Superposition): Seq[StateData]

  def run(circuit: Circuit): State

  def measure(register: QubitRegister, state: Vector): Collapsed

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => measure(circuit.register, s.vector)
      case s: Collapsed => s
    }
  }

  def runAndMeasure(circuit: Circuit, trialsCount: Int): ExperimentResult
}

object QuantumContext {
  type Vector = Array[Float]
  type Matrix = Array[Array[Float]]

  case class QuantumException(message: String) extends Exception(message)
}