package simulator

import org.scalatest.FlatSpec
import scotty.quantum.QuantumContext.{BinaryRegister, Qubit}
import scotty.quantum.{Circuit, I, Superposition, X}
import scotty.simulator.QuantumSimulator

class QuantumSimulator extends FlatSpec {
  val qs = QuantumSimulator()

  "QuantumSimulator" should "run an empty circuit" in {
    assert(qs.run(Circuit()).asInstanceOf[Superposition].qubitCount == 0)
  }

  it should "run a circuit with one qubit" in {
    assert(qs.run(Circuit().withRegister(Qubit.zero)).asInstanceOf[Superposition].qubitCount == 1)
  }

  it should "run and measure a circuit with 2 qubits" in {
    val result = qs.runAndMeasure(Circuit().withRegister(Qubit.one, Qubit.zero))

    assert(result.qubitCount == 2)
    assert(result.toBinaryRegister == BinaryRegister(1, 0))
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 1" in {
    val result = qs.runAndMeasure(Circuit(X(0)).withRegister(Qubit.one, Qubit.zero))

    assert(result.toBinaryRegister == BinaryRegister(0, 0))
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 2" in {
    val result = qs.runAndMeasure(Circuit(X(1)).withRegister(Qubit.zero, Qubit.zero))

    assert(result.toBinaryRegister == BinaryRegister(0, 1))
  }
}