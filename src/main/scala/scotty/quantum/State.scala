package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.gate.Gate
import scotty.quantum.math.MathUtils

sealed trait State {
  val qubitCount: Int
  val register: QubitRegister
}

case class Superposition(register: QubitRegister, vector: Vector) extends State {
  lazy val qubitCount: Int = if (vector.length / 2 == 0) 0 else (Math.log10(vector.length / 2) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) this
    else ctx.product(register, gate, this)

  def combine(sp: Superposition)(implicit ctx: QuantumContext): Superposition =
    if (vector.length == 0) sp
    else ctx.tensorProduct(register, this, sp)

  override def equals(obj: Any): Boolean = obj match {
    case s: Superposition => vector.toSeq == s.vector.toSeq
    case _ => super.equals(obj)
  }

  override def toString: String = s"Superposition(${vector.toList})"
}

case class Collapsed(register: QubitRegister, index: Int) extends State {
  val qubitCount: Int = register.size

  def toBinaryRegister: BinaryRegister = BinaryRegister(
    MathUtils
      .toPaddedBinary(index, register.size)
      .zipWithIndex.map(b => register.values(b._2).label.fold(b._1)(b._1.withLabel)): _*
  )

  def toHumanString: String = toBinaryRegister.values
    .zipWithIndex
    .map(p => s"${p._1.label.getOrElse(s"bit_${p._2}")}: ${p._1.toInt}")
    .mkString("\n")
}