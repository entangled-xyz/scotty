package scotty.simulator

import scotty.quantum.math.MathUtils
import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Matrix, _}
import scotty.simulator.math.RawGate
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable
import scotty.simulator.math.Implicits._

case class QuantumSimulator(random: Random) extends QuantumContext {
  type GateGen = (Seq[Qubit], Seq[Complex], Matrix) => Matrix

  private val register = ListBuffer[Qubit]()
  private var superposition = SimSuperposition()(this)
  private var collapsedState: Option[Collapsed] = None
  private val circuit = mutable.Queue[(Op, Seq[Qubit])]()
  private val gateGenerators = mutable.Map[Symbol, GateGen]()

  GateLoader.loadDefaultGens(this)

  def allocate(n: Int): Seq[Qubit] = allocate(Qubit.zero, n)

  def allocate(base: (Complex, Complex), n: Int): Seq[Qubit] = {
    (0 until n).foldLeft(register)((qs, index) => {
      superposition = superposition.parCombination(SimSuperposition(base)(this))
      qs += Qubit(index)
    }).toList
  }

  def addGateGen(name: String, f: GateGen) = gateGenerators(Symbol(name)) = f

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

      val identity = gateGenerators(Symbol("I")).apply(Seq(), Seq(), Array())

      (padTop(RawGate(identity)(this)) :+ g) ++ padBottom(RawGate(identity)(this))
    }

    pad(gate, qs).reduce((a, b) => a combine b)
  }

  def combineGates(g1: Gate, g2: Gate): Gate = RawGate(g1)(this) combine g2

  def isUnitary(g: Gate): Boolean = RawGate(g)(this).isUnitaryMatrix

  def measure(): Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val result = superposition.probabilities().foldLeft(initialIterator)((iterator, prob) => {
      val probSum = iterator._2 + prob
      val tryCollapse = (c: Int) => if (prob > 0 && random.nextDouble() <= probSum) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, probSum, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, probSum, valueOp)
      }
    })._3

    val bits = result.fold(List[Int]())(MathUtils.toBinaryPadded(_, superposition.qubitCount).toList)

    collapsedState = Some(Collapsed(bits))

    collapsedState.get
  }

  def gateMatrix(name: String, qs: Seq[Qubit], params: Seq[Complex], target: Option[TargetGate]): Matrix =
    gateGenerators(Symbol(name)).apply(qs, params, target.fold(Matrix())(_.matrix))
}

object QuantumSimulator {
  def apply(): QuantumSimulator = this(new scala.util.Random)
}