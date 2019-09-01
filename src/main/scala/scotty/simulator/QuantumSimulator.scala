package scotty.simulator

import java.util
import scotty.quantum.QuantumContext._
import scotty.quantum.gate.Gate.GateGen
import scotty.quantum.gate.StandardGate.{CPHASE00, CPHASE01, ISWAP, PSWAP}
import scotty.quantum.gate._
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.{Superposition, _}
import scotty.simulator.math.{MatrixWrapper, VectorWrapper}
import scala.collection.parallel.ExecutionContextTaskSupport
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.ExecutionContext
import scala.util.Random

case class QuantumSimulator(ec: Option[ExecutionContext], random: Random) extends QuantumContext {
  val taskSupport: Option[ExecutionContextTaskSupport] = ec.map(new ExecutionContextTaskSupport(_))

  def measure(register: QubitRegister, state: Vector): Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val rnd = random.nextDouble()

    val result = (0 until state.length / 2).foldLeft(initialIterator)((iterator, stateIndex) => {
      val abs = Complex.abs(state(2 * stateIndex), state(2 * stateIndex + 1))
      val totalProb = iterator._2 + Math.pow(abs, 2)

      val tryCollapse = (c: Int) => if (rnd <= totalProb) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, totalProb, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, totalProb, valueOp)
      }
    })

    Collapsed(register, result._3.get)
  }

  def run(circuit: Circuit): State = {
    val shouldMeasure = circuit.flattenedOps.exists(_.isInstanceOf[Measure])
    val qubitCount = circuit.register.size
    var currentState = registerToState(circuit.register)
    val steps = circuit.gates.map(g => padGate(g, qubitCount).map(g => g.matrix(this)))
    val rows = ParArray.iterate(0, currentState.length / 2)(i => i + 1)

    taskSupport.foreach(rows.tasksupport = _)

    steps.foreach(gates => {
      val finalState = Array.fill(currentState.length)(0d)

      rows.foreach(i => {
        val binaries = MathUtils.toPaddedBinaryInts(i, qubitCount)
        var offset = 0

        val finalRow = gates.foldLeft(Array.empty[Double])((row, matrix) => {
          val n = (Math.log(matrix.length) / Math.log(2)).toInt
          val slice = binaries.slice(offset, offset + n)

          val currentRow = matrix(Integer.parseInt(slice.mkString(""), 2))

          offset += n

          if (row.isEmpty) currentRow
          else VectorWrapper.tensorProduct(row, currentRow, taskSupport)
        })

        for (j <- 0 until (finalRow.length / 2)) {
          val (r, im) = Complex.product(
            finalRow(2 * j), finalRow(2 * j + 1),
            currentState(2 * j), currentState(2 * j + 1))

          finalState(2 * i) += r
          finalState(2 * i + 1) += im
        }
      })

      currentState = finalState
    })

    if (shouldMeasure) measure(circuit.register, currentState)
    else Superposition(circuit.register, currentState)
  }

  def runAndMeasure(circuit: Circuit,
                    trialsCount: Int): ExperimentResult = {
    val experiments = ParVector.fill(trialsCount)(0)

    taskSupport.foreach(experiments.tasksupport = _)

    ExperimentResult(experiments.map(_ => runAndMeasure(circuit)).toList)
  }

  def padGate(gate: Gate, qubitCount: Int): Seq[Gate] = {
    val padGate = scotty.quantum.gate.StandardGate.I
    val topPad = (0 until gate.indexes.sortWith(_ < _)(0)).map(i => padGate(i))
    val bottomPad = (gate.indexes.sortWith(_ > _)(0) until qubitCount - 1).map(i => padGate(i))

    (topPad :+ gate) ++ bottomPad
  }

  def registerToState(register: QubitRegister): Vector = {
    if (register.values.isEmpty) Array()
    else {
      register.values
        .map(q => Array(q.a.r, q.a.i, q.b.r, q.b.i))
        .reduceLeft((state, q) => VectorWrapper.tensorProduct(state, q, taskSupport))
    }
  }

  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition =
    Superposition(register, VectorWrapper.tensorProduct(sp1.vector, sp2.vector, taskSupport))

  def product(register: QubitRegister, gate: Gate, sp: Superposition): Superposition =
    Superposition(register, MatrixWrapper.product(gate.matrix(this), sp.vector))

  def densityMatrix(vector: Vector): Matrix = VectorWrapper.ketBraOuterProduct(vector)

  def isUnitary(g: Gate): Boolean = MatrixWrapper.isUnitary(g.matrix(this))

  def gateMatrix(gate: Gate): Matrix = gate match {
    case swap: SwapGate => swapMatrix(swap)
    case g: CPHASE00 => cphase0Matrix(g, g.phi, Zero())
    case g: CPHASE01 => cphase0Matrix(g, g.phi, One())
    case control: ControlGate => controlMatrix(control)
    case dagger: Dagger => MatrixWrapper.conjugateTranspose(dagger.target.matrix(this))
    case target: TargetGate => target.customMatrix.getOrElse(targetMatrix(target))
  }

  def cphase0Matrix(gate: ControlGate, phi: Double, targetBit: Bit): Matrix = {
    val minIndex = gate.indexes.min
    val controlIndex = gate.controlIndex - minIndex
    val targetIndex = gate.targetIndexes(0) - minIndex

    val qubitCount = totalQubitCount(gate)

    val finalMatrix = MatrixWrapper.identity(Math.pow(2, qubitCount).toInt)

    for (i <- finalMatrix.indices) {
      val binaries = MathUtils.toPaddedBinary(i, qubitCount).toArray

      if (binaries(controlIndex).isInstanceOf[Zero] && binaries(targetIndex) == targetBit) {
        val c = Complex.e(phi)
        finalMatrix(i)(2 * i) = c.r
        finalMatrix(i)(2 * i + 1) = c.i
      }
    }

    finalMatrix
  }

  /**
    * Generates a matrix based on the top-level control gate and nested control and target child gates.
    *
    * First, it generates an array of arrays. Each array is a binary representation of the decimal state vector index.
    * For example, a vector of length 2 can be represented with the following matrix:
    *
    * Array(
    *   Array(0, 0), Array(0, 1),
    *   Array(1, 0), Array(1, 1)
    * )
    *
    * Second, for each top-level array we check if control bits are triggered and if they are then we apply the final
    * target gate to the target qubits.
    *
    * @param gate control gate that this method generates a matrix for
    * @return final matrix representing the control gate acting on all involved qubits
    */
  def controlMatrix(gate: ControlGate): Matrix = {
    val minIndex = gate.indexes.min

    val normalizedControlIndexes = gate.controlIndexes.map(_ - minIndex)
    val normalizedTargetIndexes = gate.targetIndexes.map(_ - minIndex)

    val qubitCount = totalQubitCount(gate)
    val stateCount = Math.pow(2, qubitCount).toInt

    val finalMatrix = Array.ofDim[Vector](stateCount)

    for (i <- 0 until stateCount) {
      val binaries = MathUtils.toPaddedBinary(i, qubitCount).toArray

      val allControlsTrigger = binaries.zipWithIndex.forall(b => {
        if (normalizedControlIndexes.contains(b._2))
          if (b._1.isInstanceOf[One]) true else false
        else true
      })

      finalMatrix(i) = if (allControlsTrigger) {
        val ntis = normalizedTargetIndexes
        val filledNtis = if (ntis.length > 1) ntis(0) to ntis.last else ntis

        val targetRegister = QubitRegister(filledNtis.map(i => Qubit(binaries(i).toBasisState)): _*)

        val gateTargetProduct = MatrixWrapper.product(
          gate.finalTarget.matrix(this),
          registerToState(targetRegister))

        type LabeledVector = (Vector, Option[String])

        binaries
          .zipWithIndex
          .map {
            case (_, index) if filledNtis.contains(index) => gateTargetProduct -> Some("target")
            case (binary, _) => binary.toBasisState.toDouble -> None
          }
          .foldLeft(Seq[LabeledVector]()) {
            case (acc, item) if item._2.contains("target") && acc.exists(_._2.contains("target")) => acc
            case (acc, item) => acc :+ item
          }
          .map(_._1)
          .reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2, taskSupport))
      } else {
        binaries
          .map(b => b.toBasisState.toDouble)
          .reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2, taskSupport))
      }
    }

    finalMatrix
  }

  def totalQubitCount(gate: Gate): Int = {
    val sortedControlIndexes = gate.indexes.sorted

    val gapQubitCount = (sortedControlIndexes.tail, sortedControlIndexes).zipped.map((a, b) => a - b - 1).sum

    gate.qubitCount + gapQubitCount
  }

  def targetMatrix(targetGate: Gate): Matrix =
    QuantumSimulator.singleQubitGateGens(targetGate.name).apply(targetGate.params)

  def swapMatrix(gate: SwapGate): Matrix = {
    val equal = (a: Vector, b: Vector) => util.Arrays.equals(a, b)
    val notEqual = (a: Vector, b: Vector) => !equal(a, b)

    def phase(s: Vector) = {
      if (equal(s, One.doubleValue)) gate match {
        case _: ISWAP => Array(Complex(0), Complex(0, 1)).toDouble
        case g: PSWAP => Array(Complex(0), Complex(Math.cos(g.phi), Math.sin(g.phi))).toDouble
        case _ => s
      } else s
    }

    val minIndex = gate.indexes.min
    val i1 = gate.index1 - minIndex
    val i2 = gate.index2 - minIndex

    val qubitCount = gate.qubitCount + Math.abs(i1 - i2) - 1

    val result = (0 until Math.pow(2, qubitCount).toInt).map(stateIndex => {
      val binaries = MathUtils.toPaddedBinary(stateIndex, qubitCount).map(_.toBasisState).toArray.toDouble
      val s1 = binaries(i1)
      val s2 = binaries(i2)

      if (notEqual(s1, s2) || notEqual(s1, Zero.doubleValue) && notEqual(s2, One.doubleValue)) {
        val i1Val = phase(s1)

        binaries(i1) = phase(s2)
        binaries(i2) = i1Val
      }

      binaries.reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2, taskSupport))
    }).toArray

    result
  }
}

object QuantumSimulator {
  import scotty.simulator.gate._

  val singleQubitGateGens: Map[String, GateGen] = Map(
    "H" -> H.matrix,
    "X" -> X.matrix,
    "Y" -> Y.matrix,
    "Z" -> Z.matrix,
    "I" -> I.matrix,
    "S" -> S.matrix,
    "T" -> T.matrix,
    "PHASE" -> PHASE.matrix,
    "PHASE0" -> PHASE0.matrix,
    "RX" -> RX.matrix,
    "RY" -> RY.matrix,
    "RZ" -> RZ.matrix
  )

  def apply(): QuantumSimulator = QuantumSimulator(None, new Random())

  def apply(random: Random): QuantumSimulator = QuantumSimulator(None, random)

  def apply(ec: ExecutionContext): QuantumSimulator = QuantumSimulator(Some(ec), new Random)

  def apply(ec: ExecutionContext, random: Random): QuantumSimulator = QuantumSimulator(Some(ec), random)
}