package scotty.quantum

import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait SuperpositionReader[T] {
  val state: Superposition

  def read: Seq[T]
}

case class StateReader(state: Superposition) extends SuperpositionReader[(Seq[Int], Complex, Double)] {
  def read: Seq[(Seq[Int], Complex, Double)] = state.vector.zipWithIndex.map(pair => (
    MathUtils.toBinaryPadded(pair._2, state.qubitCount),
    pair._1,
    Math.pow(pair._1.abs().rounded, 2)
  )).toSeq

  override def toString: String = read
    .map(p => s"|${p._1.mkString("")}>: Amplitude: ${p._2}, P: ${p._3.rounded.toPercent}%")
    .mkString("\n")
}

case class QubitReader(state: Superposition) extends SuperpositionReader[(Int, Double)] {
  def read: Seq[(Int, Double)] = {
    val ps = StateReader(state).read

    (0 until state.qubitCount).map(q => {
      (q, ps.foldLeft(0d)((sum, pair) => if (pair._1(q) == 1) sum + pair._3 else sum))
    })
  }

  override def toString: String = read
    .map(p => s"qubits[${p._1}] == 1 => probability ${p._2.rounded.toPercent}%")
    .mkString("\n")
}