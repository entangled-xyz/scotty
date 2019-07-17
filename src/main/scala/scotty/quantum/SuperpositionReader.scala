package scotty.quantum

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
