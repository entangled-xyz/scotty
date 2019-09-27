package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum._
import scotty.quantum.gate.{DefGate, GateGroup}
import scotty.quantum.math.Complex

class DefGateSpec extends FlatSpec with TestHelpers {
  val twoByTwoMatrix = Array(
    Array(0f, 0f, 1f, 0f),
    Array(1f, 0f, 0f, 0f)
  )

  val nonUnitaryMatrix = Array(
    Array(1f, 0f, 0f, 0f),
    Array(0f, 0f, 2f, 0f)
  )

  val fourByFourMatrix = Array(
    Array(1f, 0f, 0f, 0f, 0f, 0f, 0f, 0f),
    Array(0f, 0f, 1f, 0f, 0f, 0f, 0f, 0f),
    Array(0f, 0f, 0f, 0f, 1f, 0f, 0f, 0f),
    Array(0f, 0f, 0f, 0f, 0f, 0f, 1f, 0f)
  )

  val rxMatrix = (params: Seq[Double]) => {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toFloat
  }

  "Custom gate" should "be able to work with a static matrix" in {
    def myGate(index: Int) = DefGate(twoByTwoMatrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }

  it should "be able to work with a dynamic matrix and a sequence of params" in {
    def myGate(theta: Double, index: Int) = DefGate(rxMatrix, theta, index)

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
}