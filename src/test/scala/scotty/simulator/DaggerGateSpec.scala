package scotty.simulator

import org.scalatest.FlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.{DefGate, Dagger}
import scotty.quantum.math.Complex

class DaggerGateSpec extends FlatSpec with TestHelpers {
  "Dagger" should "apply conjugate transpose to the gate matrix" in {
    val gate = (index: Int) => DefGate(_ => Array(
      Array(Complex(1), Complex(0)),
      Array(Complex(0), Complex(0, -1))
    ).toFloat, Seq(), index)

    assert(Dagger(gate(0)).matrix.toList.map(_.toList) == List(
      List(Complex(1), Complex(0)),
      List(Complex(0), Complex(0, 1))).toFloat)
  }
}