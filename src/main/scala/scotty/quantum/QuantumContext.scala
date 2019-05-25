package scotty.quantum

import scotty.quantum.math.MathUtils
import scotty.quantum.QuantumContext._

trait QuantumContext {
  def allocate(n: Int): Seq[Qubit]

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit]

  def qubits: Seq[Qubit]

  def state: State

  // TODO: this should return Try
  def addToCircuit(op: Op): Unit

  // TODO: this should return Try
  def run(): Unit

  def gateMatrix(name: String): Matrix = gateMatrix(name, Seq(), Seq(), None)

  def gateMatrix(name: String, qs: Seq[Qubit], params: Seq[Complex], target: Option[TargetGate]): Matrix

  def combineGates(gate1: Gate, gate2: Gate): Gate

  def isUnitary(gate: Gate): Boolean

  def measure(): Collapsed

  def runAndMeasure(): Collapsed = {
    run()
    measure()
  }
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  def Matrix(xs: Array[Complex]*) = Array(xs: _*)

  case class QuantumException(message: String) extends Exception(message)

  case class Complex(r: Double, i: Double = 0) {
    override def toString: String = if (i == 0) s"$r" else s"$r + ${i}i"

    def abs(): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
  }

  case class Qubit(index: Int)

  object Qubit {
    def one: (Complex, Complex) = (Complex(0), Complex(1))

    def zero: (Complex, Complex) = (Complex(1), Complex(0))
  }

  sealed trait State

  trait Superposition extends State {
    implicit val computer: QuantumContext

    val vector: Vector

    lazy val qubitCount: Int = (Math.log10(vector.length) / Math.log10(2)).toInt

    def applyGate(gate: Gate): Superposition

    def probabilities(): Seq[Double] = vector.map(s => Math.pow(MathUtils.round(s.abs()), 2))

    def parCombination(state: Superposition): Superposition

    override def toString: String = probabilities().zipWithIndex.map(pair => {
      s"|${MathUtils.toBinaryPadded(pair._2, qubitCount).mkString("")}> ${pair._1}"
    }).mkString("\n")
  }

  case class Collapsed(values: List[Int]) extends State {
    override def toString: String = s"|${values.mkString("")}> 1.0"
  }

  sealed trait Op {
    implicit val computer: QuantumContext
    val qs: Seq[Qubit]
  }

  case class TargetGate(gateType: AnyRef, matrix: Matrix)

  trait Gate extends Op {
    val name = getClass.getSimpleName

    val targetGate: Option[TargetGate] = None

    val params = Seq()

    lazy val isUnitary: Boolean = computer.isUnitary(this)

    lazy val matrix = computer.gateMatrix(name.toString, qs, params, targetGate)

    def combine(gate: Gate): Gate = computer.combineGates(this, gate)

    override def toString: String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  trait CircuitGate extends Gate {
    computer.addToCircuit(this)
  }

  case class H(qs: Qubit*)(implicit val computer: QuantumContext) extends CircuitGate

  case class I(qs: Qubit*)(implicit val computer: QuantumContext) extends CircuitGate

  case class X(qs: Qubit*)(implicit val computer: QuantumContext) extends CircuitGate

  case class C(qs: Qubit*)(target: TargetGate)(implicit val computer: QuantumContext) extends CircuitGate {
    override val targetGate = Some(target)
  }

  case class CNOT(qs: Qubit*)(implicit val computer: QuantumContext) extends CircuitGate {
    override val targetGate = Some(TargetGate(X, computer.gateMatrix("X")))
  }
}