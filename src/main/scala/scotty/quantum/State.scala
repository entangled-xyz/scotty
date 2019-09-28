package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.gate.Gate
import scotty.quantum.math.MathUtils

sealed trait State {
  val qubitCount: Int
  val register: QubitRegister
}

case class Superposition(register: QubitRegister, state: Vector) extends State {
  lazy val qubitCount: Int = if (state.length / 2 == 0) 0 else (Math.log10(state.length / 2) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(implicit ctx: QuantumContext): Unit = ctx.applyGate(state, gate)

  def combine(sp: Superposition)(implicit ctx: QuantumContext): Superposition =
    if (state.length == 0) sp
    else ctx.tensorProduct(register, this, sp)

  override def toString: String = s"Superposition(${state.toList})"
}

case class Collapsed(register: QubitRegister, index: Int) extends State {
  val qubitCount: Int = register.size

  def toBinary: String = MathUtils.toPaddedBinaryString(index, register.size)

  def toBitRegister: BitRegister = BitRegister(
    MathUtils
      .toPaddedBinary(index, register.size)
      .zipWithIndex.map(b => register.values(b._2).label.fold(b._1)(b._1.withLabel)): _*)

  def toHumanString: String = toBitRegister.values.reverse
    .zipWithIndex
    .map(p => s"${p._1.label.getOrElse(s"bit_${p._2}")}: ${p._1.toInt}")
    .mkString("\n")
}