package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.BlochSphereReader.{BlochSphereData, Coordinates}
import scotty.quantum.QubitProbabilityReader.QubitData
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait StateReader[T] {
  val state: State

  lazy val isCollapsed: Boolean = state.isInstanceOf[Collapsed]

  def read: Seq[T]
}

case class StateProbabilityReader(state: State) extends StateReader[StateData] {
  def read: Seq[StateData] = state match {
    case sp: Superposition => sp.vector
      .grouped(2)
      .map(p => Complex(p(0), p(1)))
      .zipWithIndex.map(pair => StateData(
        MathUtils.toPaddedBinary(pair._2, state.qubitCount),
        pair._1,
        Math.pow(Complex.abs(pair._1), 2)
      )).toSeq
    case c: Collapsed => Seq(StateData(c.toBinaryRegister.values.toSeq, Complex(1), 1))
  }

  override def toString: String = read
    .flatMap(p => {
      val prob = p.probability.toPercent

      if (prob == 0) None
      else Some(
        s"${p.state.map(_.toHumanString).mkString("")}: " +
          s"Amplitude: ${Complex.toString(p.amplitude)}, " +
          f"P: $prob%1.2f%%"
      )
    })
    .mkString("\n")
}

object StateProbabilityReader {
  case class StateData(state: Seq[Bit], amplitude: Complex, probability: Double)
}

case class QubitProbabilityReader(register: Option[QubitRegister],
                                  state: State) extends StateReader[QubitData] {
  def read: Seq[QubitData] = {
    val stateData = StateProbabilityReader(state).read

    (0 until state.qubitCount).map(index => {
      QubitData(
        register.flatMap(_.values(index).label),
        index,
        stateData.foldLeft(0d)((sum, data) => if (data.state(index).isInstanceOf[One]) sum + data.probability else sum))
    })
  }

  def read(label: String): Option[QubitData] = read.find(q => q.label.contains(label))

  override def toString: String = read.map(_.toString).mkString("\n")
}

object QubitProbabilityReader {
  def apply(register: QubitRegister, state: State): QubitProbabilityReader = this(Some(register), state)

  def apply(state: State): QubitProbabilityReader = this(None, state)

  case class QubitData(label: Option[String], index: Int, probabilityOfOne: Double) {
    val probabilityOfZero: Double = 1 - probabilityOfOne

    override def toString: String = {
      val probZero = probabilityOfZero.toPercent
      val probOne = probabilityOfOne.toPercent

      s"${label.getOrElse(s"qubit_$index")}: " +
        f"P(0) = $probZero%1.2f%% " +
        f"P(1) = $probOne%1.2f%%"
    }
  }
}

case class BlochSphereReader(state: State)(implicit ctx: QuantumContext) extends StateReader[BlochSphereData] {
  require(state.qubitCount == 1, ErrorMessage.BlochSphereQubitCountNotOne)

  def read: Seq[BlochSphereData] = state match {
    case sp: Superposition =>
      val densityMatrix = ctx.densityMatrix(sp.vector)

      val x = 2 * densityMatrix(0)(2)
      val y = 2 * densityMatrix(1)(1)
      val z = Complex.abs(densityMatrix(0)(0), densityMatrix(0)(1)) -
        Complex.abs(densityMatrix(1)(2), densityMatrix(1)(3))

      val theta = Math.acos(z)
      val phi = if (theta == 0 || theta == Math.PI) 0 else Math.acos(x / Math.sin(theta))

      Seq(BlochSphereData(phi, theta, Coordinates(x, y, z)))
    case c: Collapsed =>
      val x = 0
      val y = 0
      val z = if (c.toBinaryRegister.values(0).isInstanceOf[Zero]) 1 else -1

      val theta = if (c.toBinaryRegister.values(0).isInstanceOf[Zero]) 0 else Math.PI
      val phi = 0

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