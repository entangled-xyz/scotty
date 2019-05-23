package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable
import scotty.simulator.math.Implicits._

case class QuantumSimulator(random: Random) extends QuantumContext {
  private val register = ListBuffer[Qubit]()
  private var mutableState = StateWithVector()(this)
  private val opsQueue = mutable.Queue[(Op, Seq[Qubit])]()

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(base: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      mutableState = mutableState.combine(StateWithVector(base)(this))
      qs += Qubit(index)
    }).toList
  }

  def qubits: Seq[Qubit] = register.toList

  def state: State = mutableState

  def add(op: Op): Unit = opsQueue.enqueue((op, op.qs))

  def run(): Unit = {
    def pad(op: Op, qs: Seq[Qubit]): Seq[Op] = {
      def padTop(op: Op): Seq[Op] = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => op)
      def padBottom(op: Op): Seq[Op] = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => op)

      (padTop(I()(this)) :+ op) ++ padBottom(I()(this))
    }

    mutableState = opsQueue
      .dequeueAll(_ => true)
        .map(pair => prepareOp(pair._1, pair._2))
        .foldLeft(mutableState)((state, op) => StateWithVector(state.applyOp(op).vector())(this))
  }

  def prepareOp(op: Op, qs: Seq[Qubit]): Op = {
    def pad(op: Op, qs: Seq[Qubit]): Seq[Op] = {
      def padTop(op: Op): Seq[Op] = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => op)
      def padBottom(op: Op): Seq[Op] = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => op)

      (padTop(I()(this)) :+ op) ++ padBottom(I()(this))
    }

    pad(op, qs).reduce((a, b) => a combine b)
  }

  def combineOps(op1: Op, op2: Op): Op = OpWithMatrix(op1)(this) combine op2

  def isUnitary(op: Op): Boolean = OpWithMatrix(op)(this).isUnitaryMatrix

  def matrixGenerator(op: Op): () => Matrix = () => {
    implicit val _ = this

    op match {
      case I(_) => Gate.I
      case X(_) => Gate.X
      case C(g, qs) =>
        val indices = qs.map(_.index)
        Gate.C(g,
          Math.abs(indices.reduceLeft(_ - _)) - 1,
          indices.sliding(2).forall { case Seq(x, y) => x > y })
      case CNOT(qs) => matrixGenerator(C(X(), qs)).apply()
    }
  }
}

object QuantumSimulator {
  def apply(): QuantumSimulator = this(new scala.util.Random)
}