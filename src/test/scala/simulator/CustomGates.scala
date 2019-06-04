package simulator

import org.scalatest.FlatSpec
import scotty.quantum.QuantumContext.{BinaryRegister, Matrix, Qubit}
import scotty.quantum._
import scotty.quantum.math.Complex
import scotty.simulator.QuantumSimulator

class CustomGates extends FlatSpec {
  val sim = QuantumSimulator()

  "Custom gate" should "negate a qubit" in {
    case class CustomGate(index: Int) extends Target {
      override def targetMatrix = Some(Array(
        Array(Complex(0), Complex(1)),
        Array(Complex(1), Complex(0))
      ))
    }

    assert(sim.runAndMeasure(Circuit(CustomGate(0))).toBinaryRegister == BinaryRegister(1))
    assert(sim.runAndMeasure(Circuit(CustomGate(0)).withRegister(Qubit.one)).toBinaryRegister == BinaryRegister(0))
  }
}