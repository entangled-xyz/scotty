package scotty.quantum

import scotty.quantum.QubitProbabilityReader.QubitResult
import scotty.quantum.StateProbabilityReader.StateResult
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait SuperpositionReader[T] {
  val state: Superposition

  def read: Seq[T]
}

case class StateProbabilityReader(state: Superposition) extends SuperpositionReader[StateResult] {
  def read: Seq[StateResult] = state.vector.zipWithIndex.map(pair => StateResult(
    MathUtils.toBinaryPadded(pair._2, state.qubitCount),
    pair._1,
    Math.pow(pair._1.abs.rounded, 2)
  )).toSeq

  override def toString: String = read
    .map(p => s"|${p.state.mkString("")}>: " +
      s"Amplitude: ${p.amplitude}, " +
      s"P: ${p.probability.rounded.toPercent}%")
    .mkString("\n")
}

object StateProbabilityReader {
  case class StateResult(state: Seq[Bit], amplitude: Complex, probability: Double)
}

case class QubitProbabilityReader(state: Superposition) extends SuperpositionReader[QubitResult] {
  def read: Seq[QubitResult] = {
    val ps = StateProbabilityReader(state).read

    (0 until state.qubitCount).map(q => {
      QubitResult(
        state.qubitRegister.values(q).label,
        q,
        ps.foldLeft(0d)((sum, pair) => if (pair.state(q) == One()) sum + pair.probability else sum))
    })
  }

  def read(label: String): Option[QubitResult] = read.find(q => q.label.exists(_ == label))

  override def toString: String = read.map(_.toString).mkString("\n")
}

object QubitProbabilityReader {
  case class QubitResult(label: Option[String], index: Int, probability: Double) {
    override def toString: String = s"${label.getOrElse(s"qubit_$index")}: " +
      s"P(0) = ${(1 - probability).rounded.toPercent}% " +
      s"P(1) = ${probability.rounded.toPercent}%"
  }
}