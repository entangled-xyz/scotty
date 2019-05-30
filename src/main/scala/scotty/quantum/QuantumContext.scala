package scotty.quantum

import scotty.quantum.math.MathUtils
import scotty.quantum.QuantumContext._
import scotty.quantum.math.MathUtils._

trait QuantumContext {
  def run(circuit: Circuit): Superposition

  def controlMatrix(gate: Control): Matrix

  def par(gate1: Gate, gate2: Gate): Matrix

  def isUnitary(gate: Gate): Boolean

  def matrix(targetGate: Target): Matrix

  def runAndMeasure(circuit: Circuit): Collapsed = {
    run(circuit).measure()
  }
}

object QuantumContext {
  type Vector = Array[Complex]
  type Matrix = Array[Array[Complex]]

  case class QuantumException(message: String) extends Exception(message)

  case class Complex(r: Double, i: Double = 0) {
    override def toString: String = s"$r ${if (i >= 0) "+ " else ""}${i}i"

    def abs(): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
  }

  case class QubitState(a: Complex, b: Complex)

  object QubitState {
    def one(): QubitState = QubitState(Complex(0), Complex(1))

    def zero(): QubitState = QubitState(Complex(1), Complex(0))
  }

  case class Circuit(initState: QubitState, ops: Op*) {
    val qubitCount = ops.flatMap(op => op.indexes).distinct.max + 1
    val initRegister = List.fill(qubitCount)(initState)
    val indexes = 0 until qubitCount

    def combine(circuit: Circuit): Circuit = Circuit(ops ++ circuit.ops: _*)

    def combine(newOps: Op*): Circuit = Circuit(ops ++ newOps: _*)
  }

  object Circuit {
    def apply(ops: Op*): Circuit = this(QubitState.zero(), ops: _*)
  }

  sealed trait State {
    def toHumanString(index: Int, qubitCount: Int, amp: Option[Complex], prob: Double) =
      s"|${MathUtils.toBinaryPadded(index, qubitCount).mkString("")}>: " +
        s"Amplitude: ${amp.getOrElse("n/a")}, " +
        s"P: ${prob.toPercent}%"
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
      .map(pair => toHumanString(pair._1, qubitCount, Some(pair._2), pair._3))
      .mkString("\n")
  }

  case class Collapsed(qubitCount: Int, index: Int) extends State {
    override def toString: String = toHumanString(index, qubitCount, None, 1)
  }

  sealed trait Op {
    val qubitCount: Int
    val indexes: Seq[Int]
  }

  sealed trait Gate extends Op {
    val name = getClass.getSimpleName

    lazy val qubitCount = indexes.length

    def isUnitary()(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

    def matrix()(implicit ctx: QuantumContext): Matrix = this match {
      case targetGate: Target => ctx.matrix(targetGate)
      case controlGate: Control => ctx.controlMatrix(controlGate)
    }

    def par(gate: Gate)(implicit ctx: QuantumContext): Matrix = ctx.par(this, gate)

    def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")
  }

  trait Target extends Gate {
    val index: Int

    val params = Seq[Double]()
    val indexes = Seq(index)
  }

  trait Control extends Gate {
    val controlIndex: Int
    val target: Gate

    lazy val indexes = controlIndex +: target.indexes
    lazy val finalTarget: Target = target match {
      case t: Target => t
      case c: Control => c.finalTarget
    }
    lazy val finalTargetIndex = finalTarget.index
    lazy val controlIndexes = indexes.filter(i => i != finalTargetIndex)
    lazy val isAsc = controlIndex < target.indexes(0)
  }

  case class Controlled(controlIndex: Int, target: Gate) extends Control

  case class H(index: Int) extends Target

  case class I(index: Int) extends Target

  case class X(index: Int) extends Target

  case class Y(index: Int) extends Target

  case class Z(index: Int) extends Target

  case class CNOT(controlIndex: Int, targetIndex: Int) extends Control {
    val target = X(targetIndex)
  }

  case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends Control {
    val target = Controlled(controlIndex2, X(targetIndex))
  }
}