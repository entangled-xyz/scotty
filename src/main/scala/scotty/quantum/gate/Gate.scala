package scotty.quantum.gate

import scotty.ErrorMessage
import scotty.quantum.Op
import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.gate.TargetGate.MatrixGen
import scotty.simulator.math.MatrixWrapper

sealed trait Gate extends Op {
  val name: String = getClass.getSimpleName

  def indicesAreUnique: Boolean = indices.distinct.size == indices.size

  def indicesAreAsc: Boolean = indices.length <= 1 || (indices, indices.tail).zipped.forall(_ <= _)
}

object TargetGate {
  type MatrixGen = Seq[Double] => Matrix
}

trait TargetGate extends Gate {
  val index: Int
  val params: Seq[Double]
  val matrixGen: MatrixGen

  lazy val indices: Seq[Int] = Seq(index)

  def matrix: Matrix = matrixGen.apply(params)

  override def toString: String = matrix.toList.map(_.toList.mkString(" ")).mkString("\n")
}

trait ControlGate extends Gate {
  val controlIndex: Int
  val target: Gate
  lazy val indices: Seq[Int] = controlIndex +: target.indices

  lazy val finalTarget: TargetGate = target match {
    case c: ControlGate => c.finalTarget
    case t: TargetGate => t
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

case class Dagger(target: TargetGate) extends TargetGate {
  val index: Int = target.index
  val params: Seq[Double] = target.params

  val matrixGen: MatrixGen = _ => MatrixWrapper.conjugateTranspose(target.matrix)
}

case class GateGroup(gates: Gate*) extends Gate {
  val indices: Seq[Int] = gates.flatMap(g => g.indices).distinct
}

case class DefGate(matrixGen: MatrixGen, params: Seq[Double], index: Int) extends TargetGate

object DefGate {
  def apply(matrix: Matrix, index: Int): DefGate = this(_ => matrix, Seq(), index)

  def apply(matrixGen: MatrixGen, param: Double, index: Int): DefGate = this(matrixGen, Seq(param), index)
}