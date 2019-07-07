package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum._
import scotty.quantum.math.Complex

class CustomGatesSpec extends FlatSpec {
  val sim = QuantumSimulator()
  val twoByTwoMatrix = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )

  val fourByFourMatrix = Array(
    Array(Complex(1), Complex(0), Complex(0), Complex(0)),
    Array(Complex(0), Complex(1), Complex(0), Complex(0)),
    Array(Complex(0), Complex(0), Complex(1), Complex(0)),
    Array(Complex(0), Complex(0), Complex(0), Complex(1))
  )

  "Custom gate" should "be able to be defined as a method" in {
    def myGate(index: Int) = CustomGate(twoByTwoMatrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }

  it should "throw IllegalArgumentException if there are too many indexes" in {
    assertThrows[IllegalArgumentException] {
      CustomGate(twoByTwoMatrix, 1, 2)
    }
  }

  it should "throw IllegalArgumentException if there are too few indexes" in {
    assertThrows[IllegalArgumentException] {
      CustomGate(fourByFourMatrix, 1)
    }
  }

  it should "throw IllegalArgumentException if gaps between indexes are not accounted for" in {
    assertThrows[IllegalArgumentException] {
      CustomGate(fourByFourMatrix, 1, 3)
    }
  }
}