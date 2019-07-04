package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.MathUtils
import scotty.quantum.math.MathUtils._

sealed trait State {
  val qubitRegister: QubitRegister

  def findQubit(label: String): Option[Qubit] = qubitRegister.values.find(q => q.label.exists(_ == label))
}

trait Superposition extends State {
  val vector: Vector

  lazy val qubitCount: Int = if (vector.length == 0) 0 else (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition

  def probabilities: Seq[Double] = vector.map(s => Math.pow(s.abs.rounded, 2))

  def combine(state: Superposition)(implicit ctx: QuantumContext): Superposition

  def measure: Collapsed

  def withRegister(register: QubitRegister): Superposition

  def toString(fullState: Boolean): String =
    if (fullState) StateProbabilityReader(this).toString else QubitProbabilityReader(this).toString

  override def toString: String = toString(true)
}

case class Collapsed(qubitRegister: QubitRegister, index: Int) extends State {
  def toBinaryRegister: BinaryRegister = BinaryRegister(
    MathUtils
      .toBinaryPadded(index, qubitRegister.size)
      .zipWithIndex.map(b => qubitRegister.values(b._2)
      .label
      .fold(b._1)(b._1.withLabel))
  )

  override def toString: String = toBinaryRegister.values
    .zipWithIndex
    .map(p => s"${p._1.label.getOrElse(s"bit_${p._2}")}: ${p._1.toInt}")
    .mkString("\n")
}