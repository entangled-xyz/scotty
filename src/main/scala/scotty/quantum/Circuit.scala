package scotty.quantum

import scotty.quantum.QuantumContext.{QuantumRegister, Qubit}

case class Circuit(register: QuantumRegister, ops: Op*) {
  val indexes = 0 until register.size

  def combine(newCircuit: Circuit): Circuit = Circuit(ops ++ newCircuit.ops: _*)

  def combine(newOps: Op*): Circuit = Circuit(ops ++ newOps: _*)

  def withRegister(newRegister: QuantumRegister): Circuit = Circuit(newRegister, ops: _*)

  def withRegister(newQubits: Qubit*): Circuit = Circuit(QuantumRegister(newQubits: _*), ops: _*)

  def isValid: Boolean = register.size >= Circuit.qubitCountFromOps(ops)

  require(isValid, "The number of qubits in the register has to be the same as the number of qubits used in all ops.")
}

object Circuit {
  val defaultState = Qubit.zero

  def apply(ops: Op*): Circuit = this(generateRegister(ops), ops: _*)

  def qubitCountFromOps(ops: Seq[Op]): Int = if (ops.isEmpty) 0 else ops.flatMap(op => op.indexes).distinct.max + 1

  def generateRegister(n: Int): QuantumRegister =
    QuantumRegister(List.fill(n)(defaultState): _*)

  def generateRegister(ops: Seq[Op]): QuantumRegister =
    QuantumRegister(List.fill(qubitCountFromOps(ops))(defaultState): _*)
}