package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait State {
  def toHumanString(index: Int, qubitCount: Int, amp: Option[Complex], prob: Double) =
    s"|${MathUtils.toBinaryPadded(index, qubitCount).mkString("")}>: " +
      s"Amplitude: ${amp.getOrElse("n/a")}, " +
      s"P: ${prob.toPercent}%"
}

trait Superposition extends State {
  val vector: Vector

  lazy val qubitCount: Int = (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition

  def probabilities(): Seq[Double] = vector.map(s => Math.pow(s.abs().roundWithPrecision, 2))

  def par(state: Superposition): Superposition

  def measure(): Collapsed

  override def toString: String = vector
    .zip(probabilities())
    .zipWithIndex
    .map(pair => (pair._2, pair._1._1, pair._1._2))
    .map(pair => toHumanString(pair._1, qubitCount, Some(pair._2), pair._3))
    .mkString("\n")
}

case class Collapsed(qubitCount: Int, index: Int) extends State {
  override def toString: String = toHumanString(index, qubitCount, None, 1)
}