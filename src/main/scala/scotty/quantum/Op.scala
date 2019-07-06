package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.Matrix

sealed trait Op {
  lazy val qubitCount: Int = indexes.length
  val indexes: Seq[Int]
}

case class CircuitConnector(circuit: Circuit) extends Op {
  val indexes: Range = circuit.indexes
}

case class Measure(index: Int) extends Op {
  val indexes: Seq[Int] = Seq(index)
}

sealed trait Gate extends Op {
  val name: String = getClass.getSimpleName

  val params: Seq[Double] = Seq[Double]()

  def isUnitary(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

  def matrix(implicit ctx: QuantumContext): Matrix = ctx.gateMatrix(this)

  def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")

  def tensorProduct(gate: Gate)(implicit ctx: QuantumContext): TargetGate = ctx.tensorProduct(this, gate)

  def indexesAreUnique: Boolean = indexes.distinct.size == indexes.size
}

trait TargetGate extends Gate {
  val customMatrix: Option[Matrix] = None

  def indexesAreAsc: Boolean = indexes.length <= 1 || (indexes, indexes.tail).zipped.forall(_ <= _)
}

trait ControlGate extends Gate {
  val controlIndex: Int
  val target: Gate

  lazy val indexes: Seq[Int] = controlIndex +: target.indexes
  lazy val finalTarget: TargetGate = target match {
    case t: TargetGate => t
    case c: ControlGate => c.finalTarget
  }
  lazy val targetIndexes: Seq[Int] = finalTarget.indexes
  lazy val controlIndexes: Seq[Int] = indexes.filter(!targetIndexes.contains(_))
  lazy val isAsc: Boolean = controlIndex < target.indexes(0)

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

class CustomGate(matrix: Matrix, customIndexes: Int*) extends TargetGate {
  val indexes: Seq[Int] = customIndexes.toSeq
  override val customMatrix: Option[Matrix] = Some(matrix)
}