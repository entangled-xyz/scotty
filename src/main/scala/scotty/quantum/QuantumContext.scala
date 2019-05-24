package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.simulator.math.MathUtils

trait QuantumContext {
  def allocate(n: Int): Seq[Qubit]

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def qubits: Seq[Qubit]

  def state: State

  // TODO: this should return Try
  def addToCircuit(op: Op): Unit

  // TODO: this should return Try
  def run(): Unit

  def combineGates(gate1: Gate, gate2: Gate): Gate

  def isUnitary(gate: Gate): Boolean
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class Complex(r: Double, i: Double = 0) {
    override def toString: String = if (i == 0) s"$r" else s"$r + ${i}i"
  }

  case class Qubit(index: Int)

  object Qubit {
    def fiftyFifty: (Complex, Complex) = (Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)))

    def one: (Complex, Complex) = (Complex(0), Complex(1))

    def zero: (Complex, Complex) = (Complex(1), Complex(0))
  }

  // State should be represented with a sum type that is
  // in superposition by default and can be collapsed by a measurement
  trait State {
    implicit val computer: QuantumContext

    lazy val qubitCount: Int = (Math.log10(vector().length) / Math.log10(2)).toInt

    def vector(): Vector

    def applyGate(gate: Gate): State

    def parCombination(state: State): State

    override def toString: String = vector().toList.mkString("\n")

    def collapsedValues: Option[Seq[Long]] = {
      val index = vector().indexWhere(v => v.r == 1)

      if (index == -1) None
      else {
        val bits = MathUtils.toBinary(index)

        Some(List.fill(qubitCount - bits.length)(0L) ++ bits)
      }
    }
  }

  trait Op {
    implicit val computer: QuantumContext
    val qs: Seq[Qubit]
  }

  trait Gate extends Op {
    lazy val isUnitary: Boolean = computer.isUnitary(this)

    def combine(gate: Gate): Gate = computer.combineGates(this, gate)

    def matrix(): Matrix

    override def toString: String = matrix().toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  trait CircuitGate extends Gate {
    computer.addToCircuit(this)
  }
}