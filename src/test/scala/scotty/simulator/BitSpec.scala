package scotty.simulator

import org.scalatest.FlatSpec
import scotty.quantum.math.Complex
import scotty.quantum.{Bit, One, Zero}

class BitSpec extends FlatSpec {
  "Bit" should "be equal to Zero() when int is 0" in {
    assert(Bit(0) == Zero())
  }

  it should "be equal to One() when int is 1" in {
    assert(Bit(1) == One())
  }

  it should "be equal to One() when vector is (0, 1)" in {
    assert(Bit(Array(Complex(0), Complex(1))) == One())
  }

  it should "be equal to Zero() when vector is (1, 0)" in {
    assert(Bit(Array(Complex(1), Complex(0))) == Zero())
  }

  it should "convert to basis state [0, 1] for One()" in {
    assert(One().toBasisState sameElements Array(Complex(0), Complex(1)))
  }

  it should "convert to basis state [1, 0] for Zero()" in {
    assert(Zero().toBasisState sameElements Array(Complex(1), Complex(0)))
  }

  it should "throw IllegalArgumentException if ints are neither zero or one" in {
    assertThrows[IllegalArgumentException] {
      Bit(2)
    }
  }

  it should "throw IllegalArgumentException if vector is not zero or one" in {
    assertThrows[IllegalArgumentException] {
      Bit(Array(Complex(2), Complex(0)))
    }
  }
}