package scotty.quantum

import scotty.quantum.QuantumContext.{BinaryRegister, Vector}
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils._

sealed trait State

trait Superposition extends State {
  val vector: Vector

  lazy val qubitCount: Int = (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition

  def probabilities: Seq[Double] = vector.map(s => Math.pow(s.abs().roundWithPrecision, 2))

  def stateProbabilities: Seq[(Seq[Int], Complex, Double)] = vector.zipWithIndex.map(pair => (
    MathUtils.toBinaryPadded(pair._2, qubitCount),
    pair._1,
    Math.pow(pair._1.abs().roundWithPrecision, 2)
  )).toSeq

  def qubitProbabilities: Seq[(Int, Double)] = {
    val ps = stateProbabilities

    (0 until qubitCount).map(q => {
      (q, ps.foldLeft(0d)((sum, pair) => if (pair._1(q) == 1) sum + pair._3 else sum))
    })
  }

  def par(state: Superposition): Superposition

  def measure: Collapsed

  def toStateString: String = stateProbabilities
    .map(p => s"|${p._1.mkString("")}>: Amplitude: ${p._2}, P: ${p._3.roundWithPrecision.toPercent}%")
    .mkString("\n")

  def toQubitString: String = qubitProbabilities
    .map(p => s"qubits[${p._1}] == 1 => probability ${p._2.roundWithPrecision.toPercent}%")
    .mkString("\n")

  def toString(showState: Boolean): String = if (showState) toStateString else toQubitString

  override def toString: String = toString(true)
}

case class Collapsed(qubitCount: Int, index: Int) extends State {
  def toBinaryRegister: BinaryRegister = BinaryRegister(MathUtils.toBinaryPadded(index, qubitCount): _*)

  override def toString: String = toBinaryRegister.values.mkString("")
}