package scotty

object ErrorMessage {
  val QubitCountMismatch = "The number of qubits in the register has to be the same as the number of qubits used in all ops."

  val GateMatrixDoesntMatchIndexes = "Gate matrix doesn't match the number of qubit indexes provided to the gate."

  val GateMatrixNotUnitary = "Gate matrix has to be unitary."

  val GateIndexesNotAsc = "Target qubit indexes have to be ascending."

  val GateIndexesNotUnique = "All qubit indexes referenced in the gate have to be unique."

  val IncorrectQubitAmplitudes = "Squares of amplitudes have to add up to 1."

  val IntToBit = "Only integers 0 and 1 can be converted to Bit."

  val VectorToBit = "Only vectors (0, 1) and (1, 0) can be converted to Bit."

  val RegisterLabelsNotUnique = "Qubit and bit register labels have to be unique."

  val SwapGateIndexCountNotTwo = "Swap gate should only have two qubit indexes."

  val BlochSphereQubitCountNotOne = "BlochSphereReader only works with the superposition of a single qubit."

  val MatrixNotSquare = "Matrix has to be a square matrix."

  val NotSameDimension = "Elements have to have the same dimension."
}
