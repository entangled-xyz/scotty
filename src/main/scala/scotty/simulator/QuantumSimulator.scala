package scotty.simulator

import scotty.quantum._
import scotty.quantum.QuantumContext._
import scotty.quantum.math.MathUtils
import scotty.simulator.math.RawGate
import scotty.simulator.math.Implicits._
import scala.collection.mutable
import scala.util.Random
import scotty.quantum.math.Complex

case class QuantumSimulator(seed: Option[Long] = None) extends QuantumContext {
  type GateGen = Seq[Double] => Matrix

  private implicit val random = seed.fold(new Random)(s => new Random(s))
  private val gateGenerators = mutable.Map[String, GateGen]()

  GateLoader.loadDefaultGens(this)

  def addGateGen(name: String, f: GateGen) = gateGenerators(name) = f

  def run(circuit: Circuit): Superposition = {
    circuit.ops
      .map(opToGate(_, circuit.indexes))
      .foldLeft(qsToSuperposition(circuit.register))((state, gate) => state.applyGate(gate)(this))
  }

  def qsToSuperposition(qs: Seq[Qubit]): Superposition =
    qs.foldLeft(SimSuperposition())((superposition, q) => superposition.par(SimSuperposition(q)))

  def opToGate(op: Op, indexes: Seq[Int]): Gate = op match {
    case g: Gate => prepareGate(g, indexes)
    case m: Measure => prepareGate(I(m.index), indexes)
  }

  def prepareGate(gate: Gate, indexes: Seq[Int]): Gate = {
    def pad(): Seq[Gate] = {
      val identityGate = RawGate(Array(
        Array(Complex(1), Complex(0)),
        Array(Complex(0), Complex(1))
      ))

      def topPad = (0 until gate.indexes.sortWith(_ < _)(0)).map(_ => identityGate)
      def bottomPad = (gate.indexes.sortWith(_ > _)(0) until indexes.length - 1).map(_ => identityGate)

      (topPad :+ gate) ++ bottomPad
    }

    pad().reduce((a, b) => RawGate(a.par(b)(this)))
  }

  def par(g1: Gate, g2: Gate): Matrix = (RawGate(g1)(this) ⊗ RawGate(g2)(this).fieldMatrix).getData

  def isUnitary(g: Gate): Boolean = RawGate(g)(this).isUnitaryMatrix

  def controlMatrix(gate: Control): Matrix = {
    def toBasisState(n: Double): Array[Complex] =
      if (n == 1) Array(Complex(0), Complex(1))
      else Array(Complex(1), Complex(0))

    val minIndex = gate.indexes.min
    val normalizedControlIndexes = gate.controlIndexes.map(i => i - minIndex)
    val sortedControlIndexes = gate.indexes.sorted
    val targetIndex = gate.finalTargetIndex - minIndex
    val gapQubitCount = (sortedControlIndexes.tail, sortedControlIndexes).zipped.map((a, b) => a - b - 1).sum
    val qubitCount = gate.qubitCount + gapQubitCount

    (0 until Math.pow(2, qubitCount).toInt).map(index => {
      val binaries = MathUtils.toBinaryPadded(index, qubitCount).map(toBasisState(_)).toArray

      val allControlsTrigger = binaries.zipWithIndex.forall(b => {
        if (normalizedControlIndexes.contains(b._2)) if (b._1 sameElements toBasisState(1)) true else false
        else true
      })

      if (allControlsTrigger) binaries(targetIndex) =
        RawGate(gate.finalTarget)(this).product(binaries(targetIndex)).getData

      binaries.map(b => SimSuperposition(b)).reduce((s1, s2) => s1 par s2).rawVector
    }).toArray
  }

  def matrix(targetGate: Target): Matrix = gateGenerators(targetGate.name).apply(targetGate.params)
}