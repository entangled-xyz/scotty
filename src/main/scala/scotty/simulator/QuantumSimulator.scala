package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.simulator.gate.I
import scotty.simulator.math.{MathUtils, RawGate}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable
import scotty.simulator.math.Implicits._

case class QuantumSimulator(random: Random) extends QuantumContext {
  private val register = ListBuffer[Qubit]()
  private var superposition = SimSuperposition()(this)
  private var collapsedState: Option[Collapsed] = None
  private val circuit = mutable.Queue[(Op, Seq[Qubit])]()

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(base: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      superposition = superposition.parCombination(SimSuperposition(base)(this))
      qs += Qubit(index)
    }).toList
  }

  def qubits: Seq[Qubit] = register.toList

  def state: State = collapsedState match {
    case Some(state) => state
    case _ => superposition
  }

  def addToCircuit(op: Op): Unit = circuit.enqueue((op, op.qs))

  def run(): Unit = {
    superposition = circuit
      .dequeueAll(_ => true)
      .flatMap {
        case (gate: Gate, qs) => Some((gate, qs))
        case _ => None
      }
      .map(pair => prepareGate(pair._1, pair._2))
      .foldLeft(superposition)((state, op) => SimSuperposition(state.applyGate(op).vector)(this))
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

  def measure(): Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val result = superposition.vector.foldLeft(initialIterator)((iterator, possibility) => {
      val currentStateP = Math.pow(MathUtils.round(possibility.abs()), 2)
      val newTotalP = iterator._2 + currentStateP
      val tryCollapse = (c: Int) => if (currentStateP > 0 && random.nextDouble() <= newTotalP) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, newTotalP, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, newTotalP, valueOp)
      }
    })._3

    val bits = result.fold(List[Long]())(MathUtils.toBinary(_).toList)

    collapsedState = Some(Collapsed(List.fill(superposition.qubitCount - bits.length)(0L) ++ bits))

    collapsedState.get
  }
}

object QuantumSimulator {
  def apply(): QuantumSimulator = this(new scala.util.Random)
}