package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.StandardGate._
import scotty.quantum._

class QuantumSimulatorSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "QuantumSimulator" should "run an empty circuit" in {
    sim.run(Circuit()) match {
      case s: Superposition => assert(s.qubitCount == 0)
      case _ =>
    }
  }

  it should "run a circuit with one qubit" in {
    sim.run(Circuit().withRegister(Qubit.zero)) match {
      case s: Superposition => assert(s.qubitCount == 1)
      case _ =>
    }
  }

  it should "run and measure a circuit with 2 qubits" in {
    val result = sim.runAndMeasure(Circuit().withRegister(Qubit.one, Qubit.zero))

    assert(result.qubitRegister.size == 2)
    assert(result.toBinaryRegister.values == Seq(One(), Zero()))
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 1" in {
    val result = sim.runAndMeasure(Circuit(X(0)).withRegister(Qubit.one, Qubit.zero))

    assert(result.toBinaryRegister.values == Seq(Zero(), Zero()))
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 2" in {
    val result = sim.runAndMeasure(Circuit(X(1)).withRegister(Qubit.zero, Qubit.zero))

    assert(result.toBinaryRegister.values == Seq(Zero(), One()))
  }

  it should "run a circuit with CircuitConnector in it" in {
    val result = sim.runAndMeasure(Circuit(X(0), CircuitConnector(Circuit(X(4))), X(2)))

    assert(result.toBinaryRegister.values == Seq(One(), Zero(), One(), Zero(), One()))
  }

  it should "return a collapsed state after being measured" in {
    sim.run(Circuit(X(0)).withRegister(Qubit.zero)) match {
      case s: Superposition =>  assert(s.measure.toBinaryRegister.values == Seq(One()))
      case _ =>
    }
  }

  it should "automatically get measured if there's a Measure op" in {
    sim.run(Circuit(X(0), Measure(0)).withRegister(Qubit.zero)) match {
      case s: Collapsed => assert(s.toBinaryRegister.values == Seq(One()))
      case _ =>
    }
  }

  it should "throw IllegalArgumentException if the number of custom qubits is less than op qubits" in {
    assertThrows[IllegalArgumentException] {
      sim.run(Circuit(X(1)).withRegister(Qubit.zero))
    }
  }

  it should "work if the number of custom qubits is greater than op qubits" in {
    sim.run(Circuit(X(0), Measure(0)).withRegister(Qubit.zero, Qubit.zero, Qubit.zero, Qubit.zero)) match {
      case s: Collapsed => assert(s.toBinaryRegister.values == Seq(One(), Zero(), Zero(), Zero()))
      case _ =>
    }
  }
}