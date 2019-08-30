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

//  def tensorProduct(gate: Gate)(implicit ctx: QuantumContext): TargetGate = ctx.tensorProduct(this, gate)

  def indexesAreUnique: Boolean = indexes.distinct.size == indexes.size

  def indexesAreAsc: Boolean = indexes.length <= 1 || (indexes, indexes.tail).zipped.forall(_ <= _)
}

object Gate {
  type GateGen = Seq[Double] => Array[Array[Double]]
}

trait TargetGate extends Gate {
  val customMatrix: Option[Matrix] = None
}

trait ControlGate extends Gate {
  val controlIndex: Int
  val target: Gate
  lazy val indexes: Seq[Int] = controlIndex +: target.indexes

  lazy val finalTarget: Gate = target match {
    case c: ControlGate => c.finalTarget
    case t: TargetGate => t
  }

  lazy val targetIndexes: Seq[Int] = finalTarget.indexes
  lazy val controlIndexes: Seq[Int] = indexes.filter(!targetIndexes.contains(_))

  require(indexesAreUnique, ErrorMessage.GateIndexesNotUnique)
}

trait SwapGate extends TargetGate {
  val index1: Int
  val index2: Int

  override lazy val indexes: Seq[Int] = {
    if (index1 > index2) Seq(index2, index1) else Seq(index1, index2)
  }

  require(indexes.size == 2, ErrorMessage.SwapGateIndexCountNotTwo)
  require(indexesAreAsc, ErrorMessage.GateIndexesNotAsc)
  require(indexesAreUnique, ErrorMessage.GateIndexesNotUnique)
}

case class Controlled(controlIndex: Int, target: Gate) extends ControlGate

case class Dagger(target: Gate) extends TargetGate {
  val indexes: Seq[Int] = target.indexes
}