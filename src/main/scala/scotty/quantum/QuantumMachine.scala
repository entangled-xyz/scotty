package scotty.quantum

import scotty.quantum.QuantumMachine.{Complex, Op, Qubit, State}
import scala.util.Try

trait QuantumMachine {
  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def indexedQubits: Seq[(Int, Qubit)]

  def qubits: Seq[Qubit]

  def state: State

  def find(qubit: Qubit): Option[(Int, Qubit)]

  def indexOf(qubit: Qubit): Int

  def applyOp(qs: Qubit)(op: Op): State

  def applyOp(qs: Qubit*)(op: Op): Try[State]
}

object QuantumMachine {
  case class QuantumException(message: String) extends Exception(message)

  case class Complex(r: Double, i: Double = 0)

  trait Qubit {
    val id: String
  }

  trait Op {
    val qubitCount: Int
    val isUnitary: Boolean
    val data: Array[Array[Complex]]

    def combine(op: Op): Op

//    require(isUnitary, "Gate has to be represented with a unitary matrix.")
  }

  trait State {
    val data: Array[Complex]
  }
}