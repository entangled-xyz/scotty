package scotty.simulator

import scotty.Config
import scotty.quantum.QuantumContext._
import scotty.quantum.gate.Gate.GateGen
import scotty.quantum.gate.StandardGate.{CPHASE00, CPHASE01, ISWAP, PSWAP}
import scotty.quantum.gate._
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.{Superposition, _}
import scotty.simulator.math.{MatrixWrapper, VectorWrapper}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray
import scala.util.Random

case class QuantumSimulator()(implicit random: Random = new Random) extends QuantumContext {
  def measure(register: QubitRegister, sp: Superposition): Collapsed = {
    val initialIterator = (0, 0d, None: Option[Int])
    val result = sp.probabilities.foldLeft(initialIterator)((iterator, prob) => {
      val probSum = iterator._2 + prob
      val tryCollapse = (c: Int) => if (prob > 0 && random.nextDouble() <= probSum) Some(c) else None

      iterator match {
        case (count, _, None) => (count + 1, probSum, tryCollapse(count))
        case (count, _, valueOp) => (count + 1, probSum, valueOp)
      }
    })._3

    Collapsed(register, result.get)
  }

  def run(circuit: Circuit): State = {
    val shouldMeasure = circuit.ops.exists(_.isInstanceOf[Measure])

    val qubitCount = circuit.register.size
    var state = registerToState(circuit.register)
    val steps = circuit.gates.map(g => padGate(g, qubitCount))
    val rows = ParArray.iterate(0, state.length / 2)(i => i + 1)

    steps.foreach(gates => {
      val finalState = Array.fill(state.length)(0d)

      rows.foreach(i => {
        val bs = MathUtils.toBinaryPadded(i, qubitCount)
        var offset = 0

        val finalRow = gates.foldLeft(Array.empty[Double])((row, gate) => {
          val n = (Math.log(gate.matrix(this).length) / Math.log(2)).toInt
          val slice = bs.slice(offset, offset + n).map {
            case _: One => 1
            case _: Zero => 0
          }

          val currentRow = gate.matrix(this)(Integer.parseInt(slice.mkString(""), 2))

          offset += n

          if (row.isEmpty) currentRow
          else VectorWrapper.tensorProduct(row, currentRow)
        })

        for (j <- 0 until (finalRow.length / 2)) {
          val (r, im) = Complex.product(finalRow(2 * j), finalRow(2 * j + 1), state(2 * j), state(2 * j + 1))

          finalState(2 * i) += r
          finalState(2 * i + 1) += im
        }
      })

      state = finalState
    })

//    if (shouldMeasure) measure(circuit.register, result) else result

    Superposition(state)
  }

  def padGate(gate: Gate, qubitCount: Int): Seq[Gate] = {
    val padGate = scotty.quantum.gate.StandardGate.I
    val topPad = (0 until gate.indexes.sortWith(_ < _)(0)).map(i => padGate(i))
    val bottomPad = (gate.indexes.sortWith(_ > _)(0) until qubitCount - 1).map(i => padGate(i))

    (topPad :+ gate) ++ bottomPad
  }

  def runAndMeasure(circuit: Circuit,
                    trialsCount: Int,
                    parallelismLevel: Int = Config.DefaultSimulatorParallelism): ExperimentResult = {
    val experiments = ParVector.fill(trialsCount)(0)

    experiments.tasksupport = new ForkJoinTaskSupport(
      new java.util.concurrent.ForkJoinPool(parallelismLevel)
    )

    ExperimentResult(experiments.map(_ => runAndMeasure(circuit)).toList)
  }

  def registerToState(register: QubitRegister): Array[Double] = {
    register.values
      .map(q => Array(q.a.r, q.a.i, q.b.r, q.b.i))
      .reduceLeft((state, q) => VectorWrapper.tensorProduct(state, q))
  }

//  def opToGate(op: Op, qubitCount: Int): collection.Seq[Gate] = op match {
//    case c: CircuitConnector => c.circuit.ops.flatMap(o => opToGate(o, qubitCount))
//    case g: Gate => Seq(prepareGate(g, qubitCount))
//    case m: Measure => Seq(prepareGate(StandardGate.I(m.index), qubitCount))
//  }
//
//  def tensorProduct(g1: Gate, g2: Gate): TargetGate = RawGate(
//    (MatrixWrapper(g1.matrix(this)) ⊗ MatrixWrapper.fieldMatrix(g2.matrix(this))).getData
//  )
//
//  def tensorProduct(sp1: Superposition, sp2: Superposition): Superposition = Superposition(
//    (VectorWrapper(sp1.vector) ⊗ VectorWrapper.fieldVector(sp2.vector)).getData
//  )
//
//  def product(gate: Gate, sp: Superposition): Superposition = Superposition(
//    (MatrixWrapper(gate.matrix(this)) * VectorWrapper.fieldVector(sp.vector)).getData
//  )
//
//  def outerProduct(sp1: Superposition, sp2: Superposition): Matrix = {
//    VectorWrapper.fieldVector(sp1.vector).outerProduct(VectorWrapper.fieldVector(sp2.vector)).getData
//  }
//
//  def densityMatrix(qubit: Qubit): Matrix = {
//    val state = Superposition(qubit)
//
//    outerProduct(state, Superposition(VectorWrapper.conjugate(state.vector).getData))
//  }

//  def isUnitary(g: Gate): Boolean = MatrixWrapper(g.matrix(this)).isUnitaryMatrix

  def gateMatrix(gate: Gate): Array[Array[Double]] = gate match {
    case swap: SwapGate => swapMatrix(swap)
    case g: CPHASE00 => cphase0Matrix(g, g.phi, Zero())
    case g: CPHASE01 => cphase0Matrix(g, g.phi, One())
    case control: ControlGate => controlMatrix(control)
    case dagger: Dagger => MatrixWrapper.conjugateTranspose(dagger.target.matrix(this))
    case target: TargetGate => target.customMatrix.getOrElse(targetMatrix(target))
  }

  def cphase0Matrix(gate: ControlGate, phi: Double, targetBit: Bit): Array[Array[Double]] = {
    val minIndex = gate.indexes.min
    val controlIndex = gate.controlIndex - minIndex
    val targetIndex = gate.targetIndexes(0) - minIndex

    val qubitCount = totalQubitCount(gate)

    val finalMatrix = MatrixWrapper.identity(Math.pow(2, qubitCount).toInt)

    for (i <- finalMatrix.indices) {
      val binaries = MathUtils.toBinaryPadded(i, qubitCount).toArray

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
  def controlMatrix(gate: ControlGate): Array[Array[Double]] = {
    val minIndex = gate.indexes.min

    val normalizedControlIndexes = gate.controlIndexes.map(_ - minIndex)
    val normalizedTargetIndexes = gate.targetIndexes.map(_ - minIndex)

    val qubitCount = totalQubitCount(gate)
    val stateCount = Math.pow(2, qubitCount).toInt

    val finalMatrix = Array.ofDim[Vector](stateCount)

    for (i <- 0 until stateCount) {
      val binaries = MathUtils.toBinaryPadded(i, qubitCount).toArray

      val allControlsTrigger = binaries.zipWithIndex.forall(b => {
        if (normalizedControlIndexes.contains(b._2))
          if (b._1.isInstanceOf[One]) true else false
        else true
      })

      finalMatrix(i) = if (allControlsTrigger) {
        val ntis = normalizedTargetIndexes
        val filledNtis = if (ntis.length > 1) ntis(0) to ntis.last else ntis

        val targetRegister = QubitRegister(filledNtis.map(i => Qubit(binaries(i).toBasisState)): _*)

        val gateTargetProduct = MatrixWrapper.product(gate.finalTarget.matrix(this), registerToState(targetRegister))

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
          .reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2))
      } else {
        binaries.map(b => b.toBasisState.toDouble).reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2))
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

  def swapMatrix(gate: SwapGate): Array[Array[Double]] = {
    def phase(s: Bit) = {
      if (s.isInstanceOf[One]) gate match {
        case _: ISWAP => Superposition(Complex(0), Complex(0, 1))
        case g: PSWAP => Superposition(Complex(0), Complex(Math.cos(g.phi), Math.sin(g.phi)))
        case _ => Superposition(s.toBasisState.toDouble)
      } else Superposition(s.toBasisState.toDouble)
    }.vector

    val minIndex = gate.indexes.min
    val i1 = gate.index1 - minIndex
    val i2 = gate.index2 - minIndex

    val qubitCount = gate.qubitCount + Math.abs(i1 - i2) - 1

    val result = (0 until Math.pow(2, qubitCount).toInt).map(stateIndex => {
      val binaries = MathUtils.toBinaryPadded(stateIndex, qubitCount).map(_.toBasisState).toArray.toDouble
//      val s1 = Bit(binaries(i1))
//      val s2 = Bit(binaries(i2))
//
//      if (s1 != s2 || s1 != Zero() && s2 != One()) {
//        val i1Val = phase(s1)
//
//        binaries(i1) = phase(s2)
//        binaries(i2) = i1Val
//      }

      binaries.reduce((s1, s2) => VectorWrapper.tensorProduct(s1, s2))
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
}