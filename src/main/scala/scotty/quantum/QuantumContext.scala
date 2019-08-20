package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.quantum.gate.{Gate, TargetGate}
import scotty.quantum.math.Complex

trait QuantumContext {
  def run(circuit: Circuit): State

  def gateMatrix(gate: Gate): Matrix

//  def tensorProduct(gate1: Gate, gate2: Gate): TargetGate

//  def tensorProduct(sp1: Superposition, sp2: Superposition): Superposition

//  def product(gate: Gate, sp: Superposition): Superposition

//  def outerProduct(sp1: Superposition, sp2: Superposition): Matrix

//  def densityMatrix(qubit: Qubit): Matrix

//  def isUnitary(gate: Gate): Boolean

  def measure(register: QubitRegister, sp: Superposition): Collapsed

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => measure(circuit.register, s)
      case s: Collapsed => s
    }
  }

  def runAndMeasure(circuit: Circuit, trialsCount: Int, parallelismLevel: Int): ExperimentResult
}

object QuantumContext {
  type Vector = Array[Double]
  type Matrix = Array[Array[Double]]

  case class QuantumException(message: String) extends Exception(message)
}