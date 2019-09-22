package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex

object PHASE0 {
  def matrix(params: Seq[Double]): Matrix = {
    val phi = params(0)

    Array(
      Array(Complex.e(phi), Complex(0)),
      Array(Complex(0), Complex(1))
    ).toFloat
  }
}