package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.StandardGate.{H, RY, RZ, X}
import scotty.quantum.math.Complex
import scotty.quantum._

class SuperpositionReaderSpec extends FlatSpec with TestHelpers {
  "StateProbabilityReader" should "read correct StateData" in {
    val sp = QuantumSimulator().run(Circuit(H(0))).asInstanceOf[Superposition]
    val data0 = StateProbabilityReader(sp).read(0)
    val data1 = StateProbabilityReader(sp).read(1)

    assert(data0.state === Seq(Zero()))
    assert(data0.amplitude === Complex(fiftyPercent, 0))
    assert(data0.probability === 0.5d)

    assert(data1.state === Seq(One()))
    assert(data1.amplitude === Complex(fiftyPercent, 0))
    assert(data1.probability === 0.5d)
  }

  "QubitProbabilityReader" should "read correct StateData" in {
    val sp = QuantumSimulator().run(Circuit(H(0), X(1))).asInstanceOf[Superposition]
    val data0 = QubitProbabilityReader(sp).read(0)
    val data1 = QubitProbabilityReader(sp).read(1)

    assert(data0.index === 0)
    assert(data0.probability === 0.5d)

    assert(data1.index === 1)
    assert(data1.probability === 1d)
  }

  "BlochSphereReader" should "read correct StateData" in {
    val sp = QuantumSimulator().run(Circuit(RY(Math.PI / 4, 0), RZ(Math.PI / 8, 0))).asInstanceOf[Superposition]
    val data = BlochSphereReader(sp).read(0)

    assert(data.coordinates.x === 0.65)
    assert(data.coordinates.y === 0.27)
    assert(data.coordinates.z === 0.71)
    assert(data.phi === Math.PI / 8)
    assert(data.theta === Math.PI / 4)
  }

  it should "throw IllegalArgumentException if there are more than one qubit in the superposition" in {
    assertThrows[IllegalArgumentException] {
      val sp = QuantumSimulator().run(Circuit(X(0), X(1))).asInstanceOf[Superposition]

      BlochSphereReader(sp).read(0)
    }
  }
}
