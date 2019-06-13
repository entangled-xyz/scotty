package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.MathUtils
import scotty.quantum.math.MathUtils._

sealed trait State

trait Superposition extends State {
  val vector: Vector

  lazy val qubitCount: Int = if (vector.length == 0) 0 else (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition

  def probabilities: Seq[Double] = vector.map(s => Math.pow(s.abs.rounded, 2))

  def par(state: Superposition): Superposition

  def measure: Collapsed

  def toString(fullState: Boolean): String =
    if (fullState) StateProbabilityReader(this).toString else QubitProbabilityReader(this).toString

  override def toString: String = toString(true)
}

case class Collapsed(qubitCount: Int, index: Int) extends State {
  def toBinaryRegister: BinaryRegister = BinaryRegister(MathUtils.toBinaryPadded(index, qubitCount))

  override def toString: String = toBinaryRegister.values.mkString("")
}