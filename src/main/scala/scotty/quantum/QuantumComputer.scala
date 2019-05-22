package scotty.quantum

import scotty.quantum.QuantumComputer._

trait QuantumComputer {
  def allocate(n: Int): Seq[Qubit]

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def qubits: Seq[Qubit]

  def state: State

  def add(op: Op, qs: Seq[Qubit]): Seq[(Op, Seq[Qubit])]

  def matrixForOpType(op: Op): () => Matrix

  def combine(op: Op): Op

  def isUnitary(op: Op): Boolean

  def qubitCount(op: Op): Int
}

object QuantumComputer {
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class Complex(r: Double, i: Double = 0)

  case class Qubit(index: Int)

  object Qubit {
    def fiftyFifty: (Complex, Complex) = (Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)))

    def one: (Complex, Complex) = (Complex(0), Complex(1))

    def zero: (Complex, Complex) = (Complex(1), Complex(0))
  }

  trait State {
    val vector: Array[Complex]
  }

  trait Op {
    implicit val computer: QuantumComputer

    val qs: Seq[Qubit]

    lazy val qubitCount: Int = computer.qubitCount(this)

    lazy val isUnitary: Boolean = computer.isUnitary(this)

    def matrix = computer.matrixForOpType(this)

    def execute(state: State): State = ??? // move to state as in `state.execute(op)`

    def addToComputer(): Unit = computer.add(this, qs)

    def combine(op: Op): Op = computer.combine(this)

    addToComputer()
  }

  case class X(qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object X {
    def apply(q: Qubit)(implicit machine: QuantumComputer): X = this(Seq(q))
  }

  case class I(qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object I {
    def apply(q: Qubit)(implicit machine: QuantumComputer): I = this(Seq(q))
    def apply()(implicit machine: QuantumComputer): I = this(Seq())
  }

  case class C(gate: Op, qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object C {
    def apply(gate: Op, controlQ: Qubit)(implicit machine: QuantumComputer): C = this(gate, controlQ +: gate.qs)
  }

  case class CNOT(qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object CNOT {
    def apply(controlQ: Qubit, targetQ: Qubit)(implicit machine: QuantumComputer): CNOT = this(Seq(controlQ, targetQ))
  }
}