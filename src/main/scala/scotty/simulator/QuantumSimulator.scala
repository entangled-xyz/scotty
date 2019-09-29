package scotty.simulator

import scotty.quantum.QuantumContext._
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.gate.StandardGate.{CCNOT, CNOT, CPHASE, CPHASE00, CPHASE01, CPHASE10, CSWAP, ISWAP, PHASE, PSWAP, S, SWAP, X}
import scotty.quantum.gate._
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.{Superposition, _}
import scotty.simulator.math.{MatrixWrapper, VectorWrapper}

import scala.collection.parallel.{ExecutionContextTaskSupport, ParIterable}
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.ExecutionContext
import scala.util.Random

case class QuantumSimulator(ec: Option[ExecutionContext], random: Random) extends QuantumContext {
  val taskSupport: Option[ExecutionContextTaskSupport] = ec.map(new ExecutionContextTaskSupport(_))

  def probabilities(sp: Superposition): Seq[StateData] = {
    val probabilities = Vector[StateData]()

    parIndices(sp.state.length).foldLeft(probabilities)((ps, i) => {
      val r = sp.state(2 * i)
      val im = sp.state(2 * i + 1)
      val p = Complex.abs(Complex.product(r, im, r, im))

      if (p > 0) ps :+ StateData(MathUtils.toPaddedBinaryString(i, sp.qubitCount), Complex(r, im), p)
      else ps
    })
  }

  def measure(register: QubitRegister, state: Vector): Collapsed = {
    val initProbData = (0, 0d, None: Option[Int])
    val rnd = random.nextDouble()

    val result = parIndices(state.length).foldLeft(initProbData)((probData, stateIndex) => {
      val abs = Complex.abs(state(2 * stateIndex), state(2 * stateIndex + 1))
      val totalProb = probData._2 + Math.pow(abs, 2)

      val tryCollapse = (i: Int) => if (rnd <= totalProb) Some(i) else None

      probData match {
        case (index, _, None) => (index + 1, totalProb, tryCollapse(index))
        case (index, _, indexOp) => (index + 1, totalProb, indexOp)
      }
    })

    Collapsed(register, result._3.get)
  }

  def run(circuit: Circuit): State = {
    val state = registerToState(circuit.register)
    val shouldMeasure = circuit.ops.exists(_.isInstanceOf[Measure])
    val iterator = parIndices(state.length / 2)

    circuit.gates.foreach(applyGate(iterator, state, _))

    if (shouldMeasure) measure(circuit.register, state)
    else Superposition(circuit.register, state)
  }

  def applyGate(state: Vector, gate: Gate): Unit = applyGate(parIndices(state.length / 2), state, gate)

  def applyGate(iterator: ParIterable[Int], state: Vector, gate: Gate): Unit = gate match {
    case g: SWAP => applyGateGroup(iterator, state, QuantumSimulator.swap.apply(g.index1, g.index2))
    case g: CSWAP => applyGateGroup(iterator, state, QuantumSimulator.cswap.apply(g.controlIndex, g.index1, g.index2))
    case g: ISWAP => applyGateGroup(iterator, state, QuantumSimulator.iswap.apply(g.index1, g.index2))
    case g: PSWAP => applyGateGroup(iterator, state, QuantumSimulator.pswap.apply(g.phi, g.index1, g.index2))
    case g: CPHASE00 =>
      applyGateGroup(iterator, state, QuantumSimulator.cphase00.apply(g.phi, g.controlIndex, g.targetIndex))
    case g: CPHASE01 =>
      applyGateGroup(iterator, state, QuantumSimulator.cphase01.apply(g.phi, g.controlIndex, g.targetIndex))
    case control: ControlGate => applyControlGate(iterator, state, control)
    case target: TargetGate => applyTargetGate(iterator, state, target.index, target.matrix)
    case _ => ???
  }

  def applyGateGroup(iterator: ParIterable[Int], state: Vector, gg: GateGroup): Unit = {
    gg.gates.foreach(g => applyGate(iterator, state, g))
  }

  def applyTargetGate(iterator: ParIterable[Int], state: Vector, index: Int, matrix: Matrix): Unit = {
    iterator.foreach(i => {
      val clearedBit = nthCleared(i, index)
      val oneIndex = 2 * clearedBit
      val zeroIndex = 2 * (clearedBit | (1 << index))

      val zeroState = (state(oneIndex), state(oneIndex + 1))
      val oneState = (state(zeroIndex), state(zeroIndex + 1))

      val newZeroState = Complex.sum(
        Complex.product(matrix(0)(0), matrix(0)(1), zeroState._1, zeroState._2),
        Complex.product(matrix(0)(2), matrix(0)(3), oneState._1, oneState._2)
      )

      val newOneState = Complex.sum(
        Complex.product(matrix(1)(0), matrix(1)(1), zeroState._1, zeroState._2),
        Complex.product(matrix(1)(2), matrix(1)(3), oneState._1, oneState._2)
      )

      state(oneIndex) = newZeroState._1
      state(oneIndex + 1) = newZeroState._2

      state(zeroIndex) = newOneState._1
      state(zeroIndex + 1) = newOneState._2
    })
  }

  def applyControlGate(iterator: ParIterable[Int], state: Vector, control: ControlGate): Unit = {
    val m = control.finalTarget.matrix
    val targetIndex = control.finalTarget.index

    iterator.foreach(i => {
      val clearedBit = nthCleared(i, targetIndex)
      val zeroIndex = 2 * clearedBit
      val oneIndex = 2 * (clearedBit | (1 << targetIndex))

      val zeroControlsTrigger = control.controlIndexes.forall(idx => ((1 << idx) & (zeroIndex / 2)) > 0)
      val oneControlsTrigger = control.controlIndexes.forall(idx => ((1 << idx) & (oneIndex / 2)) > 0)

      if (zeroControlsTrigger || oneControlsTrigger) {
        val zeroState = (state(zeroIndex), state(zeroIndex + 1))
        val oneState = (state(oneIndex), state(oneIndex + 1))

        if (zeroControlsTrigger) {
          val newZeroState = Complex.sum(
            Complex.product(m(0)(0), m(0)(1), zeroState._1, zeroState._2),
            Complex.product(m(0)(2), m(0)(3), oneState._1, oneState._2)
          )

          state(zeroIndex) = newZeroState._1
          state(zeroIndex + 1) = newZeroState._2
        }

        if (oneControlsTrigger) {
          val newOneState = Complex.sum(
            Complex.product(m(1)(0), m(1)(1), zeroState._1, zeroState._2),
            Complex.product(m(1)(2), m(1)(3), oneState._1, oneState._2)
          )

          state(oneIndex) = newOneState._1
          state(oneIndex + 1) = newOneState._2
        }
      }
    })
  }

  def nthCleared(n: Int, target: Int): Int = {
    val mask = (1 << target) - 1

    (n & mask) | ((n & ~mask) << 1)
  }

  def runExperiment(circuit: Circuit, trialsCount: Int): ExperimentResult = {
    ExperimentResult((0 until trialsCount).map(_ => run(circuit) match {
      case sp: Superposition => measure(circuit.register, sp.state)
      case c: Collapsed => c
    }).toList)
  }

  def parIndices(stateLength: Int): ParArray[Int] = {
    val is = ParArray.iterate(0, stateLength / 2)(i => i + 1)

    taskSupport.foreach(is.tasksupport = _)

    is
  }

  def registerToState(register: QubitRegister): Vector = {
    if (register.values.isEmpty) Array()
    else if (register.values.forall(_ == Qubit.zero)) {
      val state = Array.fill(2 * Math.pow(2, register.size).toInt)(0f)
      state(0) = 1f
      state
    }
    else if (register.values.forall(_ == Qubit.one)) {
      val state = Array.fill(2 * Math.pow(2, register.size).toInt)(0f)
      state(state.length - 2) = 1f
      state
    } else {
      register.values
        .map(q => Array(q.a.r, q.a.i, q.b.r, q.b.i))
        .reduceLeft((state, q) => VectorWrapper.tensorProduct(state, q, taskSupport))
    }
  }

  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition =
    Superposition(register, VectorWrapper.tensorProduct(sp1.state, sp2.state, taskSupport))

  def densityMatrix(vector: Vector): Matrix = VectorWrapper.ketBraOuterProduct(vector)

  def isUnitary(g: TargetGate): Boolean = MatrixWrapper.isUnitary(g.matrix)
}

object QuantumSimulator {
  val swap = (i0: Int, i1: Int) => GateGroup(CNOT(i0, i1), CNOT(i1, i0), CNOT(i0, i1))
  val cswap = (i0: Int, i1: Int, i2: Int) => GateGroup(CNOT(i2, i1), CCNOT(i0, i1, i2), CNOT(i2, i1))
  val iswap = (i0: Int, i1: Int) => GateGroup(SWAP(i0, i1), S(i0), S(i1), CPHASE(Math.PI / 2, i0, i1))
  val pswap = (phi: Double, i0: Int, i1: Int) => GateGroup(SWAP(i0, i1), PHASE(phi, i0), PHASE(phi, i1), CPHASE(phi, i0, i1))
  val cphase00 = (phi: Double, i0: Int, i1: Int) => GateGroup(X(i0), CPHASE10(phi, i0, i1), X(i0))
  val cphase01 = (phi: Double, i0: Int, i1: Int) => GateGroup(X(i0), CPHASE(phi, i0, i1), X(i0))

  def apply(): QuantumSimulator = QuantumSimulator(None, new Random())

  def apply(random: Random): QuantumSimulator = QuantumSimulator(None, random)

  def apply(ec: ExecutionContext): QuantumSimulator = QuantumSimulator(Some(ec), new Random)

  def apply(ec: ExecutionContext, random: Random): QuantumSimulator = QuantumSimulator(Some(ec), random)
}