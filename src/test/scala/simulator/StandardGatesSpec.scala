package simulator

import org.scalatest.FlatSpec
import scotty.quantum.QuantumContext.BinaryRegister
import scotty.quantum.StandardGate.{X, _}
import scotty.quantum._
import scotty.quantum.math.Complex
import scotty.simulator.QuantumSimulator
import scotty.quantum.math.MathUtils._

class StandardGatesSpec extends FlatSpec {
  val sim = QuantumSimulator()
  val quarterTurn = Math.PI / 2
  val fiftyPercent = (Math.sqrt(2) / 2).rounded

  "CNOT" should "change target qubit when control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), CNOT(0, 1))).toBinaryRegister == BinaryRegister(1, 1))
  }

  it should "not change target qubit when control is 0" in {
    assert(sim.runAndMeasure(Circuit(CNOT(0, 1))).toBinaryRegister == BinaryRegister(0, 0))
  }

  "CCNOT" should "change target qubit when both controls are 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(2), CCNOT(0, 2, 4))).toBinaryRegister == BinaryRegister(1, 0, 1, 0 , 1))
  }

  it should "not change target qubit when one of the controls is 0" in {
    assert(sim.runAndMeasure(Circuit(X(2), CCNOT(0, 2, 4))).toBinaryRegister == BinaryRegister(0, 0, 1, 0, 0))
  }

  "H" should "set superposition to 50/50" in {
    sim.run(Circuit(H(0))) match {
      case s: Superposition => assert(QubitProbabilityReader(s).read(0).probability.rounded == 0.5)
    }
  }

  "I" should "do nothing" in {
    sim.run(Circuit(I(0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(1))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0))
        assert(s.measure.toBinaryRegister == BinaryRegister(0))
    }
  }

  "X" should "negate qubit" in {
    sim.run(Circuit(X(0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(1))
        assert(s.measure.toBinaryRegister == BinaryRegister(1))
    }
  }

  "Y" should "negate qubit" in {
    sim.run(Circuit(Y(0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(0, 0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0, 1))
        assert(s.measure.toBinaryRegister == BinaryRegister(1))
    }
  }

  "Z" should "change phase" in {
    sim.run(Circuit(Z(0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude == Complex(1, 0))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0, 0))
        assert(s.measure.toBinaryRegister == BinaryRegister(0))
    }
  }

  "RX" should "rotate qubit around X" in {
    sim.run(Circuit(RX(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude.rounded == Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude.rounded == Complex(0, -fiftyPercent))
    }
  }

  "RY" should "rotate qubit around Y" in {
    sim.run(Circuit(RY(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude.rounded == Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude.rounded == Complex(fiftyPercent, 0))
    }
  }

  "RZ" should "rotate qubit around Z" in {
    sim.run(Circuit(RZ(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude.rounded == Complex(fiftyPercent, -fiftyPercent))
        assert(StateProbabilityReader(s).read(1).amplitude == Complex(0, 0))
    }
  }
}