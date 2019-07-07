package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum._
import scotty.quantum.math.Complex
import scotty.quantum.math.MathUtils._

class CustomGatesSpec extends FlatSpec {
  val sim = QuantumSimulator()

  val quarterTurn = Math.PI / 2
  val fiftyPercent = (Math.sqrt(2) / 2).rounded

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

  val rxMatrix = (params: Seq[Double]) => {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    )
  }

  "Custom gate" should "be able to work with a static matrix" in {
    def myGate(index: Int) = CustomGate(twoByTwoMatrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }

  it should "be able to work with a dynamic matrix and a sequence of params" in {
    def myGate(theta: Double, index: Int) = CustomGate(rxMatrix, Seq(theta), Seq(index))

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude.rounded == Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude.rounded == Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  it should "be able to work with a dynamic matrix and a single param" in {
    def myGate(theta: Double, index: Int) = CustomGate(rxMatrix, theta, index)

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude.rounded == Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude.rounded == Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  it should "be able to work with a dynamic matrix, a single param, and multiple indexes" in {
    def myGate(theta: Double, index1: Int, index2: Int) = CustomGate(_ => fourByFourMatrix, theta, index1, index2)

    assert(sim.runAndMeasure(Circuit(myGate(quarterTurn, 0, 1))).toBinaryRegister.values == Seq(Zero(), Zero()))
    assert(sim.runAndMeasure(Circuit(myGate(quarterTurn, 0, 1))
      .withRegister(Qubit.one, Qubit.one)).toBinaryRegister.values == Seq(One(), One()))
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