package scotty.quantum

import scotty.quantum.QuantumContext.Qubit

case class Circuit(register: Seq[Qubit], ops: Op*) {
  val qubitCount = Circuit.qubitCountFromOps(ops)

  val indexes = 0 until qubitCount

  def combine(circuit: Circuit): Circuit = Circuit(circuit.register, ops ++ circuit.ops: _*)

  def combine(newOps: Op*): Circuit = Circuit(Circuit.generateQubits(newOps), ops ++ newOps: _*)

  def combine(newRegister: Qubit*)(newOps: Op*): Circuit = Circuit(newRegister, ops ++ newOps: _*)

  def isValid: Boolean = register.length == qubitCount

  require(isValid, "The number of qubits in the register has to be the same as the number of qubits used in all ops.")
}

object Circuit {
  val defaultState = Qubit.zero

  def apply(ops: Op*): Circuit = this(generateQubits(ops), ops: _*)

  def qubitCountFromOps(ops: Seq[Op]): Int = ops.flatMap(op => op.indexes).distinct.max + 1

  def generateQubits(n: Int): Seq[Qubit] = List.fill(n)(defaultState)

  def generateQubits(ops: Seq[Op]): Seq[Qubit] = List.fill(qubitCountFromOps(ops))(defaultState)
}