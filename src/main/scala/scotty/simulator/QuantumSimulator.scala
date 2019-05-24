package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.simulator.gate.I
import scotty.simulator.math.RawGate
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable

case class QuantumSimulator(random: Random) extends QuantumContext {
  private val register = ListBuffer[Qubit]()
  private var mutableState = StateWithVector()(this)
  private val circuit = mutable.Queue[(Op, Seq[Qubit])]()

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(base: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      mutableState = mutableState.parCombination(StateWithVector(base)(this))
      qs += Qubit(index)
    }).toList
  }

  def qubits: Seq[Qubit] = register.toList

  def state: State = mutableState

  def addToCircuit(op: Op): Unit = circuit.enqueue((op, op.qs))

  def run(): Unit = {
    mutableState = circuit
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
      def padTop(g: Gate) = (0 until qs.sortWith(_.index < _.index)(0).index).map(_ => g)
      def padBottom(g: Gate) = (qs.sortWith(_.index > _.index)(0).index until register.length - 1).map(_ => g)

      (padTop(RawGate(I.matrix)(this)) :+ g) ++ padBottom(RawGate(I.matrix)(this))
    }

    pad(gate, qs).reduce((a, b) => a combine b)
  }

  def combineGates(g1: Gate, g2: Gate): Gate = RawGate(g1)(this) combine g2

  def isUnitary(g: Gate): Boolean = RawGate(g)(this).isUnitaryMatrix
}

object QuantumSimulator {
  def apply(): QuantumSimulator = this(new scala.util.Random)
}