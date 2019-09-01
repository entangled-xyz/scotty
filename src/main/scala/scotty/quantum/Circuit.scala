package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.gate.Gate

case class Circuit(register: QubitRegister, ops: Op*) {
  val indexes: Range = 0 until register.size

  def combine(newCircuit: Circuit): Circuit = Circuit(ops ++ newCircuit.ops: _*)

  def combine(newOps: Op*): Circuit = Circuit(ops ++ newOps: _*)

  def withRegister(newRegister: QubitRegister): Circuit = Circuit(newRegister, ops: _*)

  def withRegister(newQubits: Qubit*): Circuit = Circuit(QubitRegister(newQubits: _*), ops: _*)

  def isValid: Boolean = register.size >= Circuit.qubitCountFromOps(ops.toSeq)

  def flattenedOps: Seq[Op] = ops.collect {
    case cc: CircuitConnector => cc.circuit.flattenedOps
    case op: Op => Seq(op)
  }.flatten.toSeq

  def gates: Seq[Gate] = ops.collect {
    case g: Gate => Seq(g)
    case cc: CircuitConnector => cc.circuit.gates
  }.flatten.toSeq

  require(isValid, ErrorMessage.QubitCountMismatch)
}

object Circuit {
  val defaultState: Qubit = Qubit.zero

  def apply(ops: Op*): Circuit = this(generateRegister(ops.toSeq), ops: _*)

  def qubitCountFromOps(ops: Seq[Op]): Int = if (ops.isEmpty) 0 else ops.flatMap(op => op.indexes).distinct.max + 1

  def generateRegister(n: Int): QubitRegister =
    QubitRegister(List.fill(n)(defaultState): _*)

  def generateRegister(ops: Seq[Op]): QubitRegister =
    QubitRegister(List.fill(qubitCountFromOps(ops))(defaultState): _*)
}