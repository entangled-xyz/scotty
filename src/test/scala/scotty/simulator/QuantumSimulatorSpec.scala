package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.gate.StandardGate._
import scotty.quantum._

class QuantumSimulatorSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "QuantumSimulator" should "run an empty circuit" in {
    sim.run(Circuit()) match {
      case s: Superposition => assert(s.qubitCount == 0)
      case _ =>
    }
  }

  it should "run a circuit with a custom register" in {
    assert(sim.run(Circuit().withRegister("0")).qubitCount == 1)
  }

  it should "run amd measure a circuit with runAndMeasure" in {
    assert(sim.runAndMeasure(Circuit().withRegister("01")).toBinary == "01")
  }

  it should "run and measure a circuit with the Measure op" in {
    assert(sim.run(Circuit(Measure(0)).withRegister("01")).asInstanceOf[Collapsed].toBinary == "01")
  }

  it should "run and measure a circuit with 2 qubits" in {
    assert(sim.runAndMeasure(Circuit().withRegister("10")).toBinary == "10")
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 1" in {
    assert(sim.runAndMeasure(Circuit(X(0)).withRegister("00")).toBinary == "01")
  }

  it should "run and measure a 2 qubit circuit with an X gate applied to qubit 2" in {
    assert(sim.runAndMeasure(Circuit(X(1)).withRegister("00")).toBinary == "10")
  }

  it should "throw IllegalArgumentException if the number of custom qubits is less than op qubits" in {
    assertThrows[IllegalArgumentException] {
      sim.run(Circuit(X(1)).withRegister("0"))
    }
  }

  it should "work if the number of custom qubits is greater than op qubits" in {
    assert(sim.run(Circuit(X(0), Measure(0)).withRegister("0000")).asInstanceOf[Collapsed].toBinary == "0001")
  }

  it should "work with a register defined by a string" in {
    assert(sim.runAndMeasure(Circuit(X(1)).withRegister("1111")).toBinary == "1101")
  }

  it should "work with a register defined by an int" in {
    assert(sim.runAndMeasure(Circuit(X(1)).withRegister(15)).toBinary == "1101")
  }

  it should "run and measure multiple trials of the same circuit" in {
    assert(sim.runExperiment(Circuit(X(0)), 5).trials.length == 5)
  }
}