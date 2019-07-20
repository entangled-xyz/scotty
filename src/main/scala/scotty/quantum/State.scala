package scotty.quantum

import org.apache.commons.math3.complex.Complex
import scotty.quantum.QuantumContext.Vector
import scotty.quantum.gate.Gate
import scotty.quantum.math.MathUtils
import scotty.quantum.math.MathUtils._
import scala.util.Random

sealed trait State

case class Superposition(vector: Vector) extends State {
  lazy val qubitCount: Int = if (vector.length == 0) 0 else (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) this
    else ctx.product(gate, this)

  def probabilities: Seq[Double] = vector.map(s => Math.pow(s.abs.rounded, 2))

  def combine(sp: Superposition)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) sp
    else ctx.tensorProduct(this, sp)

  def toString(fullState: Boolean): String =
    if (fullState) StateProbabilityReader(this).toString else QubitProbabilityReader(this).toString

  override def toString: String = toString(true)
}

object Superposition {
  def apply()(implicit random: Random): Superposition = this(Array[Complex]())

  def apply(q: Qubit)(implicit random: Random): Superposition = this(Array(q.a, q.b))

  def apply(a: Complex, b: Complex)(implicit random: Random): Superposition = this(Array(a, b))

  def apply(state: Superposition)(implicit random: Random): Superposition = this(state.vector)
}

case class Collapsed(register: QubitRegister, index: Int) extends State {
  def toBinaryRegister: BinaryRegister = BinaryRegister(
    MathUtils
      .toBinaryPadded(index, register.size)
      .zipWithIndex.map(b => register.values(b._2).label.fold(b._1)(b._1.withLabel)): _*
  )

  override def toString: String = toBinaryRegister.values
    .zipWithIndex
    .map(p => s"${p._1.label.getOrElse(s"bit_${p._2}")}: ${p._1.toInt}")
    .mkString("\n")
}