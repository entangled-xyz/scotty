package scotty.simulator

import scotty.quantum.QuantumComputer
import scotty.quantum.QuantumComputer._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable

case class QuantumSim(random: Random) extends QuantumComputer {
  private val register = ListBuffer[Qubit]()
  private var mutableState = SimState() // TODO: change this to val
  private val opsQueue = mutable.Queue[(Op, Seq[Qubit])]()

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(qubitState: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      mutableState = if (mutableState.vector.length == 0) SimState(qubitState) else mutableState ⊗ SimState(qubitState)
      qs += Qubit(index)
    }).toList
  }

  def qubits: Seq[Qubit] = register.toList

  def state: State = mutableState

  def queuedOps: Seq[(Op, Seq[Qubit])] = opsQueue.toList

  // may be this should return `Unit` and State should be represented with a sum type that is
  // in superposition by default and can be collapsed by a measurement
  def add(op: Op, qs: Seq[Qubit]): Seq[(Op, Seq[Qubit])] = {
    opsQueue.enqueue((op, qs))

//    val stepOp = qubits.map(q => {
//      if (q == qubit) op
//      else SimOp(I.data)(this)
//    }).reduce(_ combine _)
//
//    mutableState = SimOp(stepOp.data)(this) * SimState(state.data)
//
//    state

    queuedOps
  }

  def run: State = {
    val steps = opsQueue.dequeueAll(_ => true).map(pair => generateStep(pair._1, pair._2))

    steps.foldLeft(mutableState)((state, step) => {
      val totalOp = step.reduce((a, b) => a combine b)

      // totalOp kron totalOp

      state
    })

    println(steps)

    mutableState
  }

  def generateStep(op: Op, qs: Seq[Qubit]): Seq[Op] = {
    def padTop(op: Op): Seq[Op] = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => op)
    def padBottom(op: Op): Seq[Op] = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => op)

    (padTop(I()(this)) :+ op) ++ padBottom(I()(this))
  }

//  def applyOp(qs: Qubit*)(op: Op): Try[State] = {
//    case class OrderIterator(index: Int = -1, ordered: Boolean = true)
//
//    if (qs.length < op.qubitCount) {
//      Failure(QuantumException("Not enough qubits to execute operation"))
//    } else if (!qs.foldLeft(OrderIterator())((previous, q) => {
//      if (previous.index < indexOf(q) && previous.ordered) {
//        OrderIterator(indexOf(q))
//      } else {
//        OrderIterator(indexOf(q), ordered = false)
//      }
//    }).ordered) {
//      Failure(QuantumException("Qubits not ordered."))
//    } else if (qs.length > op.qubitCount) {
//      Failure(QuantumException("Too many qubits for provided operation"))
//    } else {
//      Success(state) // TODO: add proper multiplication for state
//    }
//  }

  def combine(op: Op): Op = ??? // JoinedGate((this ⊗ JoinedGate(op.data).fieldMatrix).getData)

  def isUnitary(op: Op): Boolean = ??? // (T * toMatrix(this.data)).round == identity

  def qubitCount(op: Op): Int = ??? // (Math.log10(rowCount) / Math.log10(2)).toInt

  def matrixForOpType(op: Op): () => Matrix = () => op match {
    case I(_) => Gate.I
    case X(_) => Gate.X
    case C(gate, qs) =>
      val indices = qs.map(_.index)
      Gate.C(gate,
        Math.abs(indices.reduceLeft(_ - _)) - 1,
        indices.sliding(2).forall { case Seq(x, y) => x > y })
  }
}

object QuantumSim {
  def apply(): QuantumSim = this(new scala.util.Random)
}