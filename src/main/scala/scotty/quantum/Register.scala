package scotty.quantum

sealed trait Register[T] {
  val values: Seq[T]

  def size: Int = values.length
}

case class QuantumRegister(values: Seq[Qubit]) extends Register[Qubit]

case class BinaryRegister(values: Seq[Bit]) extends Register[Bit]