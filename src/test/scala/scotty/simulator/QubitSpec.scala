package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.Qubit
import scotty.quantum.math.Complex

class QubitSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "Qubit" should "throw IllegalArgumentException if amplitudes are invalid" in {
    assertThrows[IllegalArgumentException] {
      Qubit(Complex(0.5), Complex(0.5))
    }
  }
}