package scotty.quantum

import scotty.quantum.QuantumContext.Matrix

sealed trait Op {
  lazy val qubitCount = indexes.length
  val indexes: Seq[Int]
}

case class CircuitConnector(circuit: Circuit) extends Op {
  val indexes = circuit.indexes
}

case class Measure(index: Int) extends Op {
  val indexes = Seq(index)
}

sealed trait Gate extends Op {
  val name = getClass.getSimpleName

  def isUnitary(implicit ctx: QuantumContext): Boolean = ctx.isUnitary(this)

  def matrix(implicit ctx: QuantumContext): Matrix = this match {
    case swap: QubitSwap => ctx.swapMatrix(swap)
    case control: Control => ctx.controlMatrix(control)
    case target: Target => targetMatrix.getOrElse(ctx.targetMatrix(target))
  }

  def targetMatrix: Option[Matrix] = None

  def par(gate: Gate)(implicit ctx: QuantumContext): Matrix = ctx.par(this, gate)

  def toString(implicit ctx: QuantumContext): String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")
}

trait Target extends Gate {
  val index1: Int

  val params = Seq[Double]()
  val indexes = Seq(index1)

  def isReversed: Boolean = false
}

trait Control extends Gate {
  val controlIndex: Int
  val target: Gate

  lazy val indexes = controlIndex +: target.indexes
  lazy val finalTarget: Target = target match {
    case t: Target => t
    case c: Control => c.finalTarget
  }
  lazy val targetIndexes = finalTarget.indexes
  lazy val controlIndexes = indexes.filter(!targetIndexes.contains(_))
  lazy val isAsc = controlIndex < target.indexes(0)
}

trait QubitSwap extends Target {
  val index2: Int

  override val indexes = Seq(index1, index2)

  override def isReversed: Boolean = index1 > index2
}