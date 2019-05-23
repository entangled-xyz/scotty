package scotty.quantum

import scotty.quantum.QuantumComputer._
import scotty.simulator.QuantumSimulator
import scotty.simulator.math.MathUtils

trait QuantumComputer {
  def allocate(n: Int): Seq[Qubit]

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def qubits: Seq[Qubit]

  def state: State

  // TODO: this should return Try
  def add(op: Op): Unit

  // TODO: this should return Try
  def run: Unit

  def matrixGenerator(gate: Op): () => Matrix

  def combineOps(op1: Op, op2: Op): Op

  def isUnitary(op: Op): Boolean
}

object QuantumComputer {
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
    implicit val computer: QuantumComputer

    val vector: Vector

    lazy val qubitCount: Int = (Math.log10(vector.length) / Math.log10(2)).toInt

    def applyOp(op: Op): State

    override def toString: String = vector.toList.mkString("\n")

    def collapsedValues: Option[Seq[Long]] = {
      val index = vector.indexWhere(v => v.r == 1)

      if (index == -1) None
      else {
        val bits = MathUtils.toBinary(index)

        Some(List.fill(qubitCount - bits.length)(0L) ++ bits)
      }
    }
  }

  trait Op {
    implicit val computer: QuantumComputer

    val qs: Seq[Qubit]

    lazy val isUnitary: Boolean = computer.isUnitary(this)

    lazy val matrixGenerator: () => Matrix = computer.matrixGenerator(this)

    def initAndAddToComputer[T <: Op](): T = {
      computer.add(this)
      this.asInstanceOf[T]
    }

    def addToComputer(): Unit = computer.add(this)

    def combine(op: Op): Op = computer.combineOps(this, op)

    override def toString: String = matrixGenerator.apply().toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  case class X(qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object X {
    def apply(q: Qubit)(implicit machine: QuantumComputer): X = this(Seq(q)).initAndAddToComputer()
    def apply()(implicit machine: QuantumComputer): X = this(Seq())
  }

  case class I(qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object I {
    def apply(q: Qubit)(implicit machine: QuantumComputer): I = this(Seq(q)).initAndAddToComputer()
    def apply()(implicit machine: QuantumComputer): I = this(Seq())
  }

  case class C(gate: Op, qs: Seq[Qubit])(implicit val computer: QuantumComputer) extends Op
  object C {
//    def apply(gate: Op, controlQ: Qubit)(implicit machine: QuantumComputer): C =
//      this(gate, controlQ +: gate.qs).initAndAddToComputer()
    def apply(gate: Op, controlQ: Qubit, targetQ: Qubit)(implicit machine: QuantumComputer): C =
      this(gate, Seq(controlQ, targetQ)).initAndAddToComputer()
  }

  case class CNOT(controlQ: Qubit, targetQ: Qubit)(implicit val computer: QuantumComputer) extends Op {
    val qs: Seq[Qubit] = Seq(controlQ, targetQ)
  }
}