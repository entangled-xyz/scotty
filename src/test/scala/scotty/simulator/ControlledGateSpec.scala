package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.gate.StandardGate.{X, _}
import scotty.quantum._
import scotty.quantum.gate.Controlled

class ControlledGateSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "Controlled gate" should "change target if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), Controlled(0, X(1)))).toBinaryRegister.values == Seq(One(), One()))
  }

  it should "not change target if control is 0" in {
    assert(sim.runAndMeasure(Circuit(Controlled(0, X(1)))).toBinaryRegister.values == Seq(Zero(), Zero()))
  }

  it should "change a target N qubits away down the register if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), Controlled(0, X(2)))).toBinaryRegister.values == Seq(One(), Zero(), One()))
  }

  it should "change a target N qubits away up the register if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(3), Controlled(3, X(1)))).toBinaryRegister.values ==
      Seq(Zero(), One(), Zero(), One()))
  }

  it should "work with chained controlled gates" in {
    assert(sim.runAndMeasure(Circuit(X(4), X(2), Controlled(4, Controlled(2, X(0))))).toBinaryRegister.values ==
      Seq(One(), Zero(), One(), Zero(), One()))
  }

  it should "work with a two-qubit SWAP gate" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(1), Controlled(0, SWAP(1, 2)))).toBinaryRegister.values ==
      Seq(One(), Zero(), One()))
  }

  it should "work with a two-qubit SWAP gate with a gap" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(1), Controlled(0, SWAP(1, 3)))).toBinaryRegister.values ==
      Seq(One(), Zero(), Zero(), One()))
  }

  it should "work with a two-qubit reversed SWAP gate with a gap" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(3), Controlled(0, SWAP(3, 1)))).toBinaryRegister.values ==
      Seq(One(), One(), Zero(), Zero()))
  }

  it should "work with a two-qubit SWAP gate while being part of the gap" in {
    assert(sim.runAndMeasure(Circuit(X(2), X(4), Controlled(2, SWAP(1, 4)))).toBinaryRegister.values ==
      Seq(Zero(), One(), One(), Zero(), Zero()))
  }

  it should "work with a two-qubit reversed SWAP gate while being part of the gap" in {
    assert(sim.runAndMeasure(Circuit(X(2), X(4), Controlled(2, SWAP(4, 1)))).toBinaryRegister.values ==
      Seq(Zero(), One(), One(), Zero(), Zero()))
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(Controlled(1, X(1))))
    }
  }
}