package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.StandardGate.{X, _}
import scotty.quantum._
import scotty.quantum.math.Complex

class StandardGatesSpec extends FlatSpec with TestHelpers {
  "CNOT" should "change target qubit when control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), CNOT(0, 1))).toBinary == "11")
  }

  it should "not change target qubit when control is 0" in {
    assert(sim.runAndMeasure(Circuit(CNOT(0, 1))).toBinary == "00")
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(CNOT(0, 0)))
    }
  }

  "CCNOT" should "change target qubit when both controls are 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(2), CCNOT(0, 2, 4))).toBinary == "10101")
  }

  it should "not change target qubit when one of the controls is 0" in {
    assert(sim.runAndMeasure(Circuit(X(2), CCNOT(0, 2, 4))).toBinary == "00100")
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(CCNOT(0, 1, 0)))
    }
  }

  "H" should "set superposition to 50/50" in {
    assert(QubitProbabilityReader(sim.run(Circuit(H(0)))).read(0).probabilityOfOne === 0.5d)
  }

  "I" should "do nothing" in {
    val r = StateProbabilityReader(sim.run(Circuit(I(0)))).read(0)

    assert(r.state == "0")
    assert(r.amplitude == Complex(1))
    assert(r.probability == 1d)
  }

  "X" should "negate qubit" in {
    val r = StateProbabilityReader(sim.run(Circuit(X(0)))).read(0)

    assert(r.state == "1")
    assert(r.amplitude == Complex(1))
    assert(r.probability == 1d)
  }

  "Y" should "negate qubit" in {
    val circuit = Circuit(Y(0))

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0, 1))
        assert(StateProbabilityReader(s).read(0).probability == 1d)
      case _ =>
    }
  }

  "Z" should "change phase" in {
    val circuit = Circuit(X(0), Z(0))

    sim.run(circuit) match {
      case s: Superposition =>
        println(s)
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(-1, 0))
        assert(StateProbabilityReader(s).read(0).probability == 1d)
      case _ =>
    }
  }

  "CZ" should "change phase if control is 1" in {
    val circuit = Circuit(X(1), CZ(1, 0))

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(1, 0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(1, 0))
        assert(StateProbabilityReader(s).read(2).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(3).amplitude == Complex(0, 0))
        assert(sim.measure(circuit.register, s.state).toBinaryRegister.values == Seq(Zero(), One()))
      case _ =>
    }
  }

  it should "not change phase if control is 0" in {
    val circuit = Circuit(CZ(1, 0))

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(1, 0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(2).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(3).amplitude == Complex(0, 0))
        assert(sim.measure(circuit.register, s.state).toBinaryRegister.values == Seq(Zero(), Zero()))
      case _ =>
    }
  }

  "S" should "change phase" in {
    val circuit = Circuit(S(0)).withRegister(Qubit.one)

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, 1))
        assert(sim.measure(circuit.register, s.state).toBinaryRegister.values == Seq(One()))
      case _ =>
    }
  }

  "T" should "change phase" in {
    val circuit = Circuit(T(0)).withRegister(Qubit.one)

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(fiftyPercent, fiftyPercent))
        assert(sim.measure(circuit.register, s.state).toBinaryRegister.values == Seq(One()))
      case _ =>
    }
  }

  "R" should "change phase" in {
    val circuit = Circuit(PHASE(Math.PI / 4, 0)).withRegister(Qubit.one)

    sim.run(circuit) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(fiftyPercent, fiftyPercent))
        assert(sim.measure(circuit.register, s.state).toBinaryRegister.values == Seq(One()))
      case _ =>
    }
  }

  "RX" should "rotate qubit around X" in {
    sim.run(Circuit(RX(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  "RY" should "rotate qubit around Y" in {
    sim.run(Circuit(RY(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(fiftyPercent, 0))
      case _ =>
    }
  }

  "RZ" should "rotate qubit around Z" in {
    sim.run(Circuit(RZ(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, -fiftyPercent))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, 0))
      case _ =>
    }
  }

  "SWAP" should "swap two nearby qubits 1 and 0" in {
    assert(sim.runAndMeasure(Circuit(X(0), SWAP(0, 1))).toBinaryRegister.values == Seq(Zero(), One()))
  }

  it should "swap two nearby qubits 0 and 1" in {
    assert(sim.runAndMeasure(Circuit(X(1), SWAP(0, 1))).toBinaryRegister.values == Seq(One(), Zero()))
  }

  it should "swap two qubits with a gap of 1 qubit" in {
    assert(sim.runAndMeasure(Circuit(X(0), SWAP(0, 2))).toBinaryRegister.values == Seq(Zero(), Zero(), One()))
  }

  it should "swap two qubits with a gap of 2 qubits" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(2), SWAP(0, 3))).toBinaryRegister.values ==
      Seq(Zero(), Zero(), One(), One()))
  }

  it should "swap two qubits in reverse with a gap of 2 qubits" in {
    assert(sim.runAndMeasure(Circuit(X(3), X(2), SWAP(3, 0))).toBinaryRegister.values ==
      Seq(One(), Zero(), One(), Zero()))
  }

  it should "have correct amplitudes" in {
    sim.run(Circuit(H(0), SWAP(0, 1))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(2).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(3).amplitude == Complex(0, 0))
      case _ =>
    }

    sim.run(Circuit(H(1), SWAP(0, 1))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(2).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(3).amplitude == Complex(0, 0))
      case _ =>
    }
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(SWAP(3, 3)))
    }
  }

  "CSWAP" should "swap two qubits when control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(1), X(2), CSWAP(1, 0, 2))).toBinaryRegister.values == Seq(One(), One(), Zero()))
  }

  it should "not swap two qubits when control is 0" in {
    assert(sim.runAndMeasure(Circuit(X(2), CSWAP(1, 0, 2))).toBinaryRegister.values == Seq(Zero(), Zero(), One()))
  }

  "PHASE" should "apply phase phi to state |1>" in {
    val s = sim.run(Circuit(H(0), PHASE(thirdTurn, 0)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(-0.35, 0.61))
  }

  "PHASE0" should "apply phase phi to state |0>" in {
    val s = sim.run(Circuit(H(0), PHASE0(thirdTurn, 0)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(-0.35, 0.61))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(0.7, 0))
  }

  "CPHASE" should "apply phase phi to |1> when control qubit is |1>" in {
    val s = sim.run(Circuit(X(0), H(1), CPHASE(thirdTurn, 0, 1)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(2).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(3).amplitude === Complex(-0.35, 0.61))
  }

  it should "not apply phase phi to state when control qubit is |0>" in {
    val s = sim.run(Circuit(H(1), CPHASE(thirdTurn, 0, 1)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(2).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(3).amplitude === Complex(0, 0))
  }

  "CPHASE10" should "apply phase phi to |0> when control qubit is |1>" in {
    val s = sim.run(Circuit(X(0), H(1), CPHASE10(thirdTurn, 0, 1)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(2).amplitude === Complex(-0.35, 0.61))
    assert(StateProbabilityReader(s).read(3).amplitude === Complex(0.7, 0))
  }

  it should "not apply phase phi to |0> when control qubit is |0>" in {
    val s = sim.run(Circuit(H(1), CPHASE10(thirdTurn, 0, 1)))

    assert(StateProbabilityReader(s).read(0).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(1).amplitude === Complex(0.7, 0))
    assert(StateProbabilityReader(s).read(2).amplitude === Complex(0, 0))
    assert(StateProbabilityReader(s).read(3).amplitude === Complex(0, 0))
  }
}