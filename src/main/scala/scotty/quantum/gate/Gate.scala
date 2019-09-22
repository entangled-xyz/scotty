package scotty.quantum.gate

import scotty.ErrorMessage
import scotty.quantum.{Op, QuantumContext}
import scotty.quantum.QuantumContext.Matrix

sealed trait Gate extends Op {
  val name: String = getClass.getSimpleName

  val params: Seq[Double] = Seq[Double]()

  def isUnitary(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

  def matrix(implicit ctx: QuantumContext): Matrix = ctx.gateMatrix(this)

  def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")

  def indicesAreUnique: Boolean = indices.distinct.size == indices.size

  def indicesAreAsc: Boolean = indices.length <= 1 || (indices, indices.tail).zipped.forall(_ <= _)
}

object Gate {
  type GateGen = Seq[Double] => Matrix
}

trait TargetGate extends Gate {
  val index: Int
  val customMatrix: Option[Matrix] = None

  lazy val indices: Seq[Int] = Seq(index)
}

trait ControlGate extends Gate {
  val controlIndex: Int
  val target: Gate
  lazy val indices: Seq[Int] = controlIndex +: target.indices

  lazy val finalTarget: Gate = target match {
    case c: ControlGate => c.finalTarget
    case t: Gate => t
  }

  lazy val targetIndexes: Seq[Int] = finalTarget.indices
  lazy val controlIndexes: Seq[Int] = indices.filter(!targetIndexes.contains(_))

  require(indicesAreUnique, ErrorMessage.GateIndexesNotUnique)
}

trait SwapGate extends Gate {
  val index1: Int
  val index2: Int

  override lazy val indices: Seq[Int] = {
    if (index1 > index2) Seq(index2, index1) else Seq(index1, index2)
  }

  require(indices.size == 2, ErrorMessage.SwapGateIndexCountNotTwo)
  require(indicesAreAsc, ErrorMessage.GateIndexesNotAsc)
  require(indicesAreUnique, ErrorMessage.GateIndexesNotUnique)
}

case class Controlled(controlIndex: Int, target: Gate) extends ControlGate

case class Dagger(target: Gate) extends Gate {
  val indices: Seq[Int] = target.indices
}