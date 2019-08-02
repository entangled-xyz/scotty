package scotty.quantum

import org.apache.commons.math3.complex.Complex
import scotty.quantum.QuantumContext._
import scotty.quantum.gate.{Gate, TargetGate}

trait QuantumContext {
  def run(circuit: Circuit): State

  def gateMatrix(gate: Gate): Matrix

  def tensorProduct(gate1: Gate, gate2: Gate): TargetGate

  def tensorProduct(sp1: Superposition, sp2: Superposition): Superposition

  def product(gate: Gate, sp: Superposition): Superposition

  def outerProduct(sp1: Superposition, sp2: Superposition): Matrix

  def densityMatrix(qubit: Qubit): Matrix

  def isUnitary(gate: Gate): Boolean

  def measure(register: QubitRegister, sp: Superposition): Collapsed

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit) match {
      case s: Superposition => measure(circuit.register, s)
      case s: Collapsed => s
    }
  }

  def runAndMeasure(circuit: Circuit, trialsCount: Int): TrialsResult
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class TrialsResult(trials: List[Collapsed]) {
    lazy val stateStats: List[Int] = trials
      .map(t => List.fill(Math.pow(2, t.qubitCount).toInt)(0).updated(t.index, 1))
      .transpose
      .map(_.sum)
  }
}