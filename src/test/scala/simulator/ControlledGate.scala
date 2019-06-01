package simulator

import org.scalatest.FlatSpec
import scotty.quantum.QuantumContext.BinaryRegister
import scotty.quantum.StandardGate.{X, _}
import scotty.quantum._
import scotty.simulator.QuantumSimulator

class ControlledGate extends FlatSpec {
  val sim = QuantumSimulator()

  "Controlled gate" should "change target if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), Controlled(0, X(1)))).toBinaryRegister == BinaryRegister(1, 1))
  }

  it should "not change target if control is 0" in {
    assert(sim.runAndMeasure(Circuit(Controlled(0, X(1)))).toBinaryRegister == BinaryRegister(0, 0))
  }

  it should "change a target N qubits away down the register if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), Controlled(0, X(2)))).toBinaryRegister == BinaryRegister(1, 0, 1))
  }

  it should "change a target N qubits away up the register if control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(3), Controlled(3, X(1)))).toBinaryRegister == BinaryRegister(0, 1, 0, 1))
  }

  it should "work with chained controlled gates" in {
    assert(sim.runAndMeasure(Circuit(X(4), X(2), Controlled(4, Controlled(2, X(0))))).toBinaryRegister ==
      BinaryRegister(1, 0, 1, 0, 1))
  }
}