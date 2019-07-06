package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum._
import scotty.quantum.math.Complex

class CustomGatesSpec extends FlatSpec {
  val sim = QuantumSimulator()
  val matrix = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )

  "Custom gate" should "be able to be defined through inheritance" in {
    case class MyGate(index: Int) extends CustomGate(matrix, index)

    assert(sim.runAndMeasure(Circuit(MyGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(MyGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }

  it should "be able to be defined as a method" in {
    def myGate(index: Int) = new CustomGate(matrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBinaryRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero()))
  }
}