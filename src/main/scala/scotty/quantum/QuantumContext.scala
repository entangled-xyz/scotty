package scotty.quantum

import scotty.quantum.math.MathUtils
import scotty.quantum.QuantumContext._
import scotty.quantum.math.MathUtils._

trait QuantumContext {
  def run(circuit: Circuit): Superposition

  def control(q: Qubit, gate: Gate): Matrix

  def par(gate1: Gate, gate2: Gate): Gate

  def isUnitary(gate: Gate): Boolean

  def matrix(targetGate: Target): Matrix

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit).measure()
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

  case class Qubit(index: Int, state: (Complex, Complex))

  object Qubit {
    def one(index: Int): Qubit = Qubit(index, (Complex(0), Complex(1)))

    def zero(index: Int): Qubit = Qubit(index, (Complex(1), Complex(0)))
  }

  case class Circuit(qs: Seq[Qubit], ops: Seq[Op])

  sealed trait State {
    def toHumanString(index: Int, qubitCount: Int, amp: Complex, prob: Double) =
      s"|${MathUtils.toBinaryPadded(index, qubitCount).mkString("")}>: $amp, P: ${prob.toPercent}%"
  }

  trait Superposition extends State {
    val vector: Vector

    lazy val qubitCount: Int = (Math.log10(vector.length) / Math.log10(2)).toInt

    def applyOp(op: Op)(implicit ctx: QuantumContext): Superposition

    def probabilities(): Seq[Double] = vector.map(s => Math.pow(s.abs().roundWithPrecision, 2))

    def par(state: Superposition): Superposition

    def measure(): Collapsed

    override def toString: String = vector
      .zip(probabilities())
      .zipWithIndex
      .map(pair => (pair._2, pair._1._1, pair._1._2))
      .map(pair => toHumanString(pair._1, qubitCount, pair._2, pair._3))
      .mkString("\n")
  }

  case class Collapsed(qubitCount: Int, index: Int) extends State {
    override def toString: String = toHumanString(index, qubitCount, Complex(1), 1)

//      s"|${values.mkString("")}> 1.0"
  }

  sealed trait Op {
    val qubitCount: Int
  }

  sealed trait Gate extends Op {
    val name = getClass.getSimpleName

    val qs: Seq[Qubit]

    def isUnitary()(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

    def matrix()(implicit ctx: QuantumContext): Matrix = this match {
      case targetGate: Target => ctx.matrix(targetGate)
      case controlGate: Controlled => ctx.control(controlGate.control, controlGate.target)
    }

    def par(gate: Gate)(implicit ctx: QuantumContext): Gate = ctx.par(this, gate)

    def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  case class Controlled(control: Qubit, target: Gate) extends Gate {
    val qs = control +: target.qs
    val qubitCount = 1 + target.qubitCount
  }

  trait Target extends Gate {
    val params = Seq[Double]()
    val qubitCount = 1
  }

  case class H(q1: Qubit) extends Target {
    lazy val qs = Seq(q1)
  }

  case class I(q1: Qubit) extends Target {
    lazy val qs = Seq(q1)
  }

  case class X(q1: Qubit) extends Target {
    lazy val qs = Seq(q1)
  }

//  case class CNOT(q1: Qubit, q2: Qubit)(implicit ctx: QuantumContext) extends Control {
//    lazy val qs = Seq(q1, q2)
//    override val targetGate = Some(TargetGate(X, ctx.gateMatrix("X")))
//  }
}