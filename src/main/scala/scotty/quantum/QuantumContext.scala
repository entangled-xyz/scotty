package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.gate.{Gate, TargetGate}

trait QuantumContext {
  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition

  def densityMatrix(vector: Vector): Matrix

  def isUnitary(gate: TargetGate): Boolean

  def probabilities(sp: Superposition): Seq[StateData]

  def applyGate(state: Vector, gate: Gate): Unit

  def run(circuit: Circuit): State

  def measure(register: QubitRegister, state: Vector): Collapsed

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => measure(circuit.register, s.state)
      case s: Collapsed => s
    }
  }

  def runExperiment(circuit: Circuit, trialsCount: Int): ExperimentResult
}

object QuantumContext {
  type Vector = Array[Float]
  type Matrix = Array[Array[Float]]

  case class QuantumException(message: String) extends Exception(message)
}