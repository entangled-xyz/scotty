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
    mutableState = opsQueue
      .dequeueAll(_ => true)
      .flatMap {
        case (gate: Gate, qs) => Some((gate, qs))
        case _ => None
      }
      .map(pair => prepareGate(pair._1, pair._2))
      .foldLeft(mutableState)((state, op) => StateWithVector(state.applyGate(op).vector())(this))
  }

  def prepareGate(gate: Gate, qs: Seq[Qubit]): Gate = {
    def pad(g: Gate, qs: Seq[Qubit]): Seq[Gate] = {
      def padTop(g: Gate): Seq[Gate] = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => g)
      def padBottom(g: Gate): Seq[Gate] = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => g)

      (padTop(I()(this)) :+ g) ++ padBottom(I()(this))
    }

    pad(gate, qs).reduce((a, b) => a combine b)
  }

  def combineGates(g1: Gate, g2: Gate): Gate= GateWithMatrix(g1)(this) combine g2

  def isUnitary(g: Gate): Boolean = GateWithMatrix(g)(this).isUnitaryMatrix

  def matrixGenerator(gate: Gate): () => Matrix = () => {
    implicit val _ = this

    gate match {
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