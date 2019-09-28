package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.{BitRegister, One, Qubit, QubitRegister, Zero}

class RegisterSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "Register" should "throw IllegalArgumentException if qubit labels have duplicates" in {
    assertThrows[IllegalArgumentException] {
      QubitRegister(Qubit.one("test"), Qubit.zero("test"))
    }
  }

  it should "throw IllegalArgumentException if bit labels have duplicates" in {
    assertThrows[IllegalArgumentException] {
      BitRegister(One("test"), Zero("test"))
    }
  }
}