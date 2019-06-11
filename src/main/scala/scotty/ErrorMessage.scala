package scotty

object ErrorMessage {
  val QubitCountError = "The number of qubits in the register has to be the same as the number of qubits used in all ops."

  val GateIndexOrderError = "Target qubit indexes have to be ascending."

  val GateIndexesAreNotUniqueError = "All qubit indexes referenced in the gate have to be unique."

  val QubitAmplitudesError = "Amplitudes have to add up to 1."

  val IntToBasisStateError = "Only classical 1s and 0s can be converted to quantum basis state."
}
