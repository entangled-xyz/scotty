package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.BlochSphereReader.{BlochSphereData, Coordinates}
import scotty.quantum.QubitProbabilityReader.QubitData
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.math.Complex.Complex
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._
import scotty.simulator.QuantumSimulator

sealed trait SuperpositionReader[T] {
  val state: Superposition

  def read: Seq[T]
}

case class StateProbabilityReader(state: Superposition) extends SuperpositionReader[StateData] {
  def read: Seq[StateData] = state.vector.zipWithIndex.map(pair => StateData(
    MathUtils.toBinaryPadded(pair._2, state.qubitCount),
    pair._1,
    Math.pow(pair._1.abs, 2)
  )).toSeq

  override def toString: String = read
    .map(p => {
      val prob = p.probability.toPercent
      s"${p.state.mkString("")}: " +
        s"Amplitude: ${Complex.toString(p.amplitude)}, " +
        f"P: $prob%1.2f%%"
    })
    .mkString("\n")
}

object StateProbabilityReader {
  case class StateData(state: Seq[Bit], amplitude: Complex, probability: Double)
}

case class QubitProbabilityReader(register: Option[QubitRegister],
                                  state: Superposition) extends SuperpositionReader[QubitData] {
  def read: Seq[QubitData] = {
    val stateData = StateProbabilityReader(state).read

    (0 until state.qubitCount).map(index => {
      QubitData(
        register.flatMap(_.values(index).label),
        index,
        stateData.foldLeft(0d)((sum, data) => if (data.state(index) == One()) sum + data.probability else sum))
    })
  }

  def read(label: String): Option[QubitData] = read.find(q => q.label.contains(label))

  override def toString: String = read.map(_.toString).mkString("\n")
}

object QubitProbabilityReader {
  def apply(register: QubitRegister, state: Superposition): QubitProbabilityReader = this(Some(register), state)
  def apply(state: Superposition): QubitProbabilityReader = this(None, state)

  case class QubitData(label: Option[String], index: Int, probability: Double) {
    override def toString: String = {
      val probZero = (1 - probability).toPercent
      val probOne = probability.toPercent

      s"${label.getOrElse(s"qubit_$index")}: " +
        f"P(0) = $probZero%1.2f%% " +
        f"P(1) = $probOne%1.2f%%"
    }
  }
}

case class BlochSphereReader(state: Superposition) extends SuperpositionReader[BlochSphereData] {
  require(state.qubitCount == 1, ErrorMessage.BlochSphereQubitCountNotOne)

  def read: Seq[BlochSphereData] = {
    val densityMatrix = QuantumSimulator().densityMatrix(Qubit(state.vector(0), state.vector(1)))

    val x = 2 * densityMatrix(0)(1).getReal
    val y = 2 * densityMatrix(1)(0).getImaginary
    val z = densityMatrix(0)(0).subtract(densityMatrix(1)(1)).abs

    val theta = Math.acos(z)
    val phi = Math.acos(x / Math.sin(theta))

    Seq(BlochSphereData(phi, theta, Coordinates(x, y, z)))
  }

  override def toString: String = {
    val state = read(0)
    val phi = Math.toDegrees(state.phi)
    val theta = Math.toDegrees(state.theta)
    val x = state.coordinates.x
    val y = state.coordinates.y
    val z = state.coordinates.z

    f"phi: $phi%1.2f°, theta: $theta%1.2f°, x: $x%1.2f, y: $y%1.2f, z: $z%1.2f"
  }
}

object BlochSphereReader {
  case class Coordinates(x: Double, y: Double, z: Double)
  case class BlochSphereData(phi: Double, theta: Double, coordinates: Coordinates)
}