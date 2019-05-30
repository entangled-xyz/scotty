package scotty.quantum

import scotty.quantum.QuantumContext.{Op, Qubit}

case class Circuit(register: Seq[Qubit], ops: Op*) {
  val qubitCount = Circuit.qubitCountFromOps(ops)

  val indexes = 0 until qubitCount

  def combine(circuit: Circuit): Circuit = Circuit(circuit.register, ops ++ circuit.ops: _*)

  def combine(newOps: Op*): Circuit = Circuit(register, ops ++ newOps: _*)

  def combine(newRegister: Qubit*)(newOps: Op*): Circuit = Circuit(newRegister, ops ++ newOps: _*)

  def isValid: Boolean = register.length == qubitCount

  require(isValid, "The number of qubits in the register has to be the same as the number of qubits used in all ops.")
}

object Circuit {
  val defaultState = Qubit.zero

  def apply(ops: Op*): Circuit = this(List.fill(qubitCountFromOps(ops))(defaultState), ops: _*)

  def qubitCountFromOps(ops: Seq[Op]) = ops.flatMap(op => op.indexes).distinct.max + 1
}