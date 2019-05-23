package scotty.simulator

import scotty.quantum.QuantumComputer
import scotty.quantum.QuantumComputer._
import scotty.simulator.QuantumSimulator.{OpWithMatrix, StateWithVector}
import scotty.simulator.math.LinearAlgebra.{MatrixTransformations, VectorTransformations}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable

case class QuantumSimulator(random: Random) extends QuantumComputer {
  private val register = ListBuffer[Qubit]()
  private var mutableState = StateWithVector()(this) // TODO: change this to val
  private val opsQueue = mutable.Queue[(Op, Seq[Qubit])]()

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      mutableState =
        if (mutableState.rawVector.length == 0) StateWithVector(qubitState)(this)
        else mutableState combine StateWithVector(qubitState)(this)
      qs += Qubit(index)
    }).toList
  }

  def qubits: Seq[Qubit] = register.toList

  def state: State = mutableState

  def add(op: Op): Unit = opsQueue.enqueue((op, op.qs))

  def run: Unit = {
    def pad(op: Op, qs: Seq[Qubit]): Seq[Op] = {
      def padTop(op: Op): Seq[Op] = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => op)
      def padBottom(op: Op): Seq[Op] = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => op)

      (padTop(I()(this)) :+ op) ++ padBottom(I()(this))
    }

    mutableState = opsQueue
      .dequeueAll(_ => true)
//      .map(op => { println(op._1.getClass); op })
      .map(pair => pad(pair._1, pair._2).reduce((a, b) => a combine b))
      .foldLeft(mutableState)((state, op) => StateWithVector(state.applyOp(op).vector)(this))
  }

  def combineOps(op1: Op, op2: Op): Op = OpWithMatrix(op1)(this) combine op2

  def isUnitary(op: Op): Boolean = ??? // (T * toMatrix(this.data)).round == identity

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
      case CNOT(q1, q2) => matrixGenerator(C(X(q2), Seq(q1, q2))).apply()
    }
  }
}

object QuantumSimulator {
  import scotty.simulator.math.Implicits._

  case class StateWithVector(rawVector: Array[Complex])
                            (implicit val computer: QuantumComputer) extends State with VectorTransformations {
    val vector = rawVector

    def combine(newState: StateWithVector): StateWithVector =
      StateWithVector((this ⊗ StateWithVector(newState.rawVector).fieldVector).getData)

    override def applyOp(newOp: Op): State = StateWithVector(OpWithMatrix(newOp).product(fieldVector).getData)
  }

  object StateWithVector {
    def apply()(implicit machine: QuantumComputer): StateWithVector = this(Array[Complex]())
    def apply(tuple: (Complex, Complex))(implicit machine: QuantumComputer): StateWithVector = this(Array(tuple._1, tuple._2))
    def apply(op: State)(implicit machine: QuantumComputer): OpWithMatrix = this(op)
  }

  case class OpWithMatrix(rawMatrix: Array[Array[Complex]])
                         (implicit val computer: QuantumComputer) extends Op with MatrixTransformations {
    val qs: Seq[Qubit] = Seq() // qubits don't matter in this context

    override lazy val matrixGenerator = () => rawMatrix

    override def combine(newOp: Op): OpWithMatrix = OpWithMatrix((this ⊗ OpWithMatrix(newOp).fieldMatrix).getData)
  }

  object OpWithMatrix {
    def apply(op: Op)(implicit machine: QuantumComputer): OpWithMatrix = this(op.matrixGenerator.apply())
  }

  def apply(): QuantumSimulator = this(new scala.util.Random)
}