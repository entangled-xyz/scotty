package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.BlochSphereReader.{BlochSphereData, Coordinates}
import scotty.quantum.QubitProbabilityReader.QubitData
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait SuperpositionReader[T] {
  val state: Superposition

  def read: Seq[T]
}

case class StateProbabilityReader(state: Superposition) extends SuperpositionReader[StateData] {
  def read: Seq[StateData] = state.vector.zipWithIndex.map(pair => StateData(
    MathUtils.toBinaryPadded(pair._2, state.qubitCount),
    pair._1,
    Math.pow(pair._1.abs.rounded, 2)
  )).toSeq

  override def toString: String = read
    .map(p => s"${p.state.mkString("")}: " +
      s"Amplitude: ${p.amplitude}, " +
      s"P: ${p.probability.rounded.toPercent}%")
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
    override def toString: String = s"${label.getOrElse(s"qubit_$index")}: " +
      s"P(0) = ${(1 - probability).rounded.toPercent}% " +
      s"P(1) = ${probability.rounded.toPercent}%"
  }
}

case class BlochSphereReader(state: Superposition) extends SuperpositionReader[BlochSphereData] {
  require(state.qubitCount == 1, ErrorMessage.BlochSphereQubitCountNotOne)

  def read: Seq[BlochSphereData] = {
    import org.apache.commons.math3.complex.{Complex => ApacheComplex}

    val a = new ApacheComplex(state.vector(0).r, state.vector(0).i)
    val b = new ApacheComplex(state.vector(1).r, state.vector(1).i)

    val theta = 2 * a.acos.abs
//    val theta = 4 * Math.acos(a.abs)
    val phi = b.divide(Math.sin(theta / 2)).log.abs

    Seq(BlochSphereData(phi.rounded, theta.rounded, coordinates(theta, phi)))
  }

  def coordinates(theta: Double, phi: Double): Coordinates = Coordinates(
    Math.cos(phi) * Math.sin(theta),
    Math.sin(theta) * Math.sin(phi),
    Math.cos(theta)
  )

  override def toString: String = {
    val state = read(0)

    s"phi: ${Math.toDegrees(state.phi).rounded}, theta: ${Math.toDegrees(state.theta).rounded}, " +
      s"x: ${state.coordinates.x.rounded}, y: ${state.coordinates.y.rounded}, z: ${state.coordinates.z.rounded}"
  }
}

object BlochSphereReader {
  case class Coordinates(x: Double, y: Double, z: Double)
  case class BlochSphereData(phi: Double, theta: Double, coordinates: Coordinates)
}