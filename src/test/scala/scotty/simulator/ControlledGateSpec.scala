package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.StandardGate.{X, _}
import scotty.quantum._
import scotty.quantum.gate.Controlled

class ControlledGateSpec extends FlatSpec with TestHelpers {
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
      Seq(One(), Zero(), One(), Zero()))
  }

  it should "work with chained controlled gates" in {
    assert(sim.runAndMeasure(Circuit(X(4), X(2), Controlled(4, Controlled(2, X(0))))).toBinaryRegister.values ==
      Seq(One(), Zero(), One(), Zero(), One()))
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(Controlled(1, X(1))))
    }
  }
}