package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum._
import scotty.quantum.math.Complex

class CustomGatesSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "Custom gate" should "negate a qubit" in {
    case class CustomGate(index1: Int) extends Target {
      override def targetMatrix = Some(Array(
        Array(Complex(0), Complex(1)),
        Array(Complex(1), Complex(0))
      ))
    }

    assert(sim.runAndMeasure(Circuit(CustomGate(0))).toBinaryRegister.values == Seq(One))
    assert(sim.runAndMeasure(Circuit(CustomGate(0)).withRegister(Qubit.one)).toBinaryRegister.values == Seq(Zero))
  }
}