package scotty.quantum

import scotty.quantum.QuantumContext._
import scotty.simulator.math.MathUtils

trait QuantumContext {
  def allocate(n: Int): Seq[Qubit]

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def qubits: Seq[Qubit]

  def state: State

  // TODO: this should return Try
  def add(op: Op): Unit

  // TODO: this should return Try
  def run(): Unit

  def matrixGenerator(gate: Gate): () => Matrix

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

    def combine(state: State): State

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

    protected def addToComputer[T <: Op](): T = {
      computer.add(this)
      this.asInstanceOf[T]
    }
  }

  trait Gate extends Op {
    lazy val matrixGenerator: () => Matrix = computer.matrixGenerator(this)
    lazy val isUnitary: Boolean = computer.isUnitary(this)

    def combine(gate: Gate): Gate = computer.combineGates(this, gate)

    override def toString: String = matrixGenerator.apply().toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  case class X(qs: Seq[Qubit])(implicit val computer: QuantumContext) extends Gate
  object X {
    def apply(q: Qubit)(implicit ctx: QuantumContext): X = this(Seq(q)).addToComputer()
    def apply()(implicit ctx: QuantumContext): X = this(Seq())
  }

  case class I(qs: Seq[Qubit])(implicit val computer: QuantumContext) extends Gate
  object I {
    def apply(q: Qubit)(implicit ctx: QuantumContext): I = this(Seq(q)).addToComputer()
    def apply()(implicit ctx: QuantumContext): I = this(Seq())
  }

  case class C(gate: Gate, qs: Seq[Qubit])(implicit val computer: QuantumContext) extends Gate
  object C {
    def apply(gate: Gate, controlQ: Qubit, targetQ: Qubit)(implicit ctx: QuantumContext): C =
      this(gate, Seq(controlQ, targetQ)).addToComputer()
  }

  case class CNOT(qs: Seq[Qubit])(implicit val computer: QuantumContext) extends Gate
  object CNOT {
    def apply(controlQ: Qubit, targetQ: Qubit)(implicit ctx: QuantumContext): CNOT =
      this(Seq(controlQ, targetQ)).addToComputer()
  }
}