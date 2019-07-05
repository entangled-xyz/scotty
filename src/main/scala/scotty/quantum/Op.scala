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

trait Gate extends Op {
  val name: String = getClass.getSimpleName

  val params: Seq[Double] = Seq[Double]()

  def isUnitary(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

  def matrix(implicit ctx: QuantumContext): Matrix = this match {
    case swap: QubitSwap => ctx.swapMatrix(swap)
    case control: Control => ctx.controlMatrix(control)
    case gate: Gate => gate.targetMatrix.getOrElse(ctx.targetMatrix(gate))
  }

  def targetMatrix: Option[Matrix] = None

  def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")

  def indexesAreUnique: Boolean = indexes.distinct.size == indexes.size
}

trait Target extends Gate {
  val index1: Int

  lazy val indexes: Seq[Int] = Seq(index1)

  def indexesAreAsc: Boolean = indexes.length <= 1 || (indexes, indexes.tail).zipped.forall(_ <= _)

  require(indexesAreAsc, ErrorMessage.TargetGateIndexOrder)
  require(indexesAreUnique, ErrorMessage.GateIndexesAreNotUnique)
}

trait Control extends Gate {
  val controlIndex: Int
  val target: Gate

  lazy val indexes: Seq[Int] = controlIndex +: target.indexes
  lazy val finalTarget: Target = target match {
    case t: Target => t
    case c: Control => c.finalTarget
  }
  lazy val targetIndexes: Seq[Int] = finalTarget.indexes
  lazy val controlIndexes: Seq[Int] = indexes.filter(!targetIndexes.contains(_))
  lazy val isAsc: Boolean = controlIndex < target.indexes(0)

  require(indexesAreUnique, ErrorMessage.GateIndexesAreNotUnique)
}

trait QubitSwap extends Target {
  val index2: Int

  override lazy val indexes: Seq[Int] = if (index1 > index2) Seq(index2, index1) else Seq(index1, index2)
}

case class RawGate(matrix: Matrix) extends Gate {
  val indexes: Seq[Int] = Seq.empty

  override lazy val qubitCount: Int = Math.sqrt(matrix.length).toInt

  override def targetMatrix = Some(matrix)
}
