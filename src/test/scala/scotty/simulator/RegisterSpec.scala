package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.{BinaryRegister, One, Qubit, QubitRegister, Zero}

class RegisterSpec extends FlatSpec {
  val sim = QuantumSimulator()

  "Register" should "throw IllegalArgumentException if qubit labels have duplicates" in {
    assertThrows[IllegalArgumentException] {
      QubitRegister(Qubit.one("test"), Qubit.zero("test"))
    }
  }

  it should "throw IllegalArgumentException if bit labels have duplicates" in {
    assertThrows[IllegalArgumentException] {
      BinaryRegister(One("test"), Zero("test"))
    }
  }
}