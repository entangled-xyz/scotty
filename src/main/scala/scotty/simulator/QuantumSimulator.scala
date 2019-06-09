package scotty.simulator

import scotty.quantum._
import scotty.quantum.QuantumContext._
import scotty.quantum.StandardGate
import scotty.quantum.math.MathUtils
import scotty.quantum.math.MathUtils._
import scotty.simulator.math.RawGate
import scotty.simulator.math.Implicits._

import scala.util.Random
import scotty.quantum.math.Complex

case class QuantumSimulator(implicit random: Random = new Random) extends QuantumContext {
  val gateGenerators = QuantumSimulator.standardGates

  def run(circuit: Circuit): State = {
    val shouldMeasure = circuit.ops.exists(op => op.isInstanceOf[Measure])

    val result = circuit.ops
      .flatMap(opToGate(_, circuit.register.size))
      .foldLeft(registerToSuperposition(circuit.register))((state, gate) => state.applyGate(gate)(this))

    if (shouldMeasure) result.measure else result
  }

  def registerToSuperposition(register: QuantumRegister): Superposition =
    register.values.foldLeft(SimSuperposition())((superposition, q) => superposition.par(SimSuperposition(q)))

  def opToGate(op: Op, qubitCount: Int): Seq[Gate] = op match {
    case c: CircuitConnector => c.circuit.ops.flatMap(o => opToGate(o, qubitCount))
    case g: Gate => Seq(prepareGate(g, qubitCount))
    case m: Measure => Seq(prepareGate(StandardGate.I(m.index), qubitCount))
  }

  def prepareGate(gate: Gate, qubitCount: Int): Gate = {
    def pad(): Seq[Gate] = {
      val identityGate = RawGate(Array(
        Array(Complex(1), Complex(0)),
        Array(Complex(0), Complex(1))
      ))

      def topPad = (0 until gate.indexes.sortWith(_ < _)(0)).map(_ => identityGate)
      def bottomPad = (gate.indexes.sortWith(_ > _)(0) until qubitCount - 1).map(_ => identityGate)

      (topPad :+ gate) ++ bottomPad
    }

    pad().reduce((a, b) => RawGate(a.par(b)(this)))
  }

  def par(g1: Gate, g2: Gate): Matrix = (RawGate(g1)(this) ⊗ RawGate(g2)(this).fieldMatrix).getData

  def isUnitary(g: Gate): Boolean = RawGate(g)(this).isUnitaryMatrix

  def controlMatrix(gate: Control): Matrix = {
    val minIndex = gate.indexes.min
    val normalizedControlIndexes = gate.controlIndexes.map(i => i - minIndex)
    val sortedControlIndexes = gate.indexes.sorted
    val gapQubitCount = (sortedControlIndexes.tail, sortedControlIndexes).zipped.map((a, b) => a - b - 1).sum
    val qubitCount = gate.qubitCount + gapQubitCount
    val normalizedTargetIndexes = gate.targetIndexes.map(_ - minIndex)

    (0 until Math.pow(2, qubitCount).toInt).map(index => {
      val binaries = MathUtils.toBinaryPadded(index, qubitCount).toArray

      val allControlsTrigger = binaries.zipWithIndex.forall(b => {
        if (normalizedControlIndexes.contains(b._2))
          if (b._1 == 1) true else false
        else true
      })

      if (allControlsTrigger) {
        val ntis = normalizedTargetIndexes
        val filledNtis = if (ntis.length > 1) {
          if (ntis(0) < ntis.last) ntis(0) to ntis.last
          else ntis(0) to ntis.last by -1
        } else ntis

        val targetRegister = QuantumRegister(filledNtis.map(i => Qubit(binaries(i).toBasisState)): _*)

        val gateTargetProduct = RawGate(gate.finalTarget)(this)
          .product(registerToSuperposition(targetRegister).vector).toArray

        binaries
          .zipWithIndex
          .map(pair => {
            if (filledNtis.contains(pair._2)) SimSuperposition(gateTargetProduct, Some("target"))
            else SimSuperposition(pair._1.toBasisState)
          })
          .foldLeft(Seq[SimSuperposition]()) { (acc, item) =>
            if (item.hasLabel("target") && acc.exists(_.hasLabel("target"))) acc
            else acc :+ item
          }
          .reduce((s1, s2) => s1 par s2)
          .rawVector
      } else {
        binaries.map(b => SimSuperposition(b.toBasisState)).reduce((s1, s2) => s1 par s2).rawVector
      }
    }).toArray
  }

  def targetMatrix(targetGate: Target): Matrix = gateGenerators(targetGate.name).apply(targetGate.params)

  def swapMatrix(gate: QubitSwap): Matrix = {
    val minIndex = gate.indexes.min
    val i1 = gate.index1 - minIndex
    val i2 = gate.index2 - minIndex

    val qubitCount = gate.qubitCount + Math.abs(i1 - i2) - 1

    (0 until Math.pow(2, qubitCount).toInt).map(index => {
      val binaries = MathUtils.toBinaryPadded(index, qubitCount).map(_.toBasisState).toArray
      val i1Val = binaries(i1)

      binaries(i1) = binaries(i2)
      binaries(i2) = i1Val

      binaries.map(b => SimSuperposition(b)).reduce((s1, s2) => s1 par s2).rawVector
    }).toArray
  }
}

object QuantumSimulator {
  import scotty.simulator.gate._

  type GateGen = Seq[Double] => Matrix

  def standardGates: Map[String, GateGen] = Map(
    "H" -> H.matrix,
    "X" -> X.matrix,
    "Y" -> Y.matrix,
    "Z" -> Z.matrix,
    "I" -> I.matrix,
    "RX" -> RX.matrix,
    "RY" -> RY.matrix,
    "RZ" -> RZ.matrix
  )
}