package scotty.quantum

import scotty.ErrorMessage

case class Circuit(register: QuantumRegister, ops: Op*) {
  val indexes: Range = 0 until register.size

  def combine(newCircuit: Circuit): Circuit = Circuit(ops ++ newCircuit.ops: _*)

  def combine(newOps: Op*): Circuit = Circuit(ops ++ newOps: _*)

  def withRegister(newRegister: QuantumRegister): Circuit = Circuit(newRegister, ops: _*)

  def withRegister(newQubits: Qubit*): Circuit = Circuit(QuantumRegister(newQubits.toSeq), ops: _*)

  def isValid: Boolean = register.size >= Circuit.qubitCountFromOps(ops.toSeq)

  require(isValid, ErrorMessage.QubitCountError)
}

object Circuit {
  val defaultState: Qubit = Qubit.zero

  def apply(ops: Op*): Circuit = this(generateRegister(ops.toSeq), ops: _*)

  def qubitCountFromOps(ops: Seq[Op]): Int = if (ops.isEmpty) 0 else ops.flatMap(op => op.indexes).distinct.max + 1

  def generateRegister(n: Int): QuantumRegister =
    QuantumRegister(List.fill(n)(defaultState))

  def generateRegister(ops: Seq[Op]): QuantumRegister =
    QuantumRegister(List.fill(qubitCountFromOps(ops))(defaultState))
}