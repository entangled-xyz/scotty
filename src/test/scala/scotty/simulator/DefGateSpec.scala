package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum._
import scotty.quantum.gate.DefGate
import scotty.quantum.math.Complex
import scotty.simulator.math.Implicits._

class DefGateSpec extends FlatSpec with TestHelpers {
  val twoByTwoMatrix = Array(
    Array(0d, 0d, 1d, 0d),
    Array(1d, 0d, 0d, 0d)
  )

  val nonUnitaryMatrix = Array(
    Array(1d, 0d, 0d, 0d),
    Array(0d, 0d, 2d, 0d)
  )

  val fourByFourMatrix = Array(
    Array(1d, 0d, 0d, 0d, 0d, 0d, 0d, 0d),
    Array(0d, 0d, 1d, 0d, 0d, 0d, 0d, 0d),
    Array(0d, 0d, 0d, 0d, 1d, 0d, 0d, 0d),
    Array(0d, 0d, 0d, 0d, 0d, 0d, 1d, 0d)
  )

  val rxMatrix = (params: Seq[Double]) => {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toDouble
  }

  "Custom gate" should "be able to work with a static matrix" in {
    def myGate(index: Int) = DefGate(twoByTwoMatrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }

  it should "be able to work with a dynamic matrix and a sequence of params" in {
    def myGate(theta: Double, index: Int) = DefGate(rxMatrix, Seq(theta), Seq(index))

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  it should "be able to work with a dynamic matrix and a single param" in {
    def myGate(theta: Double, index: Int) = DefGate(rxMatrix, theta, index)

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  it should "be able to work with a dynamic matrix, a single param, and multiple indexes" in {
    def myGate(theta: Double, index1: Int, index2: Int) = DefGate(_ => fourByFourMatrix, theta, index1, index2)

    assert(sim.runAndMeasure(Circuit(myGate(quarterTurn, 0, 1))).toBinaryRegister.values == Seq(Zero(), Zero()))
    assert(sim.runAndMeasure(Circuit(myGate(quarterTurn, 0, 1))
      .withRegister(Qubit.one, Qubit.one)).toBinaryRegister.values == Seq(One(), One()))
  }

  it should "throw IllegalArgumentException if there are too many indexes" in {
    assertThrows[IllegalArgumentException] {
      DefGate(twoByTwoMatrix, 1, 2)
    }
  }

  it should "throw IllegalArgumentException if there are too few indexes" in {
    assertThrows[IllegalArgumentException] {
      DefGate(fourByFourMatrix, 1)
    }
  }

  it should "throw IllegalArgumentException if gaps between indexes are not accounted for" in {
    assertThrows[IllegalArgumentException] {
      DefGate(fourByFourMatrix, 1, 3)
    }
  }

  it should "throw IllegalArgumentException if the matrix is not unitary" in {
    assertThrows[IllegalArgumentException] {
      DefGate(nonUnitaryMatrix, 0)
    }
  }
}