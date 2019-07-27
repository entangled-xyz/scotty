package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.gate.Gate
import scotty.quantum.math.Complex.Complex
import scotty.quantum.math.{Complex, MathUtils}

sealed trait State

case class Superposition(vector: Vector) extends State {
  lazy val qubitCount: Int = if (vector.length == 0) 0 else (Math.log10(vector.length) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) this
    else ctx.product(gate, this)

  def probabilities: Seq[Double] = vector.map(s => Math.pow(s.abs, 2))

  def combine(sp: Superposition)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) sp
    else ctx.tensorProduct(this, sp)

  def toString(fullState: Boolean): String =
    if (fullState) StateProbabilityReader(this).toString else QubitProbabilityReader(this).toString

  override def toString: String = toString(true)

  override def equals(obj: Any): Boolean = obj match {
    case s: Superposition => vector.toSeq == s.vector.toSeq
    case _ => super.equals(obj)
  }
}

object Superposition {
  def apply(): Superposition = this(Array[Complex]())

  def apply(q: Qubit): Superposition = this(Array(q.a, q.b))

  def apply(bit: Bit): Superposition = this(bit.toBasisState)

  def apply(a: Complex, b: Complex): Superposition = this(Array(a, b))

  def apply(state: Superposition): Superposition = this(state.vector)

  def one: Superposition = this(Complex(0), Complex(1))

  def zero: Superposition = this(Complex(1), Complex(0))
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