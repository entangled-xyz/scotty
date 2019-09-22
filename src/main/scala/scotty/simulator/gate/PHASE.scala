package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex

object PHASE {
  def matrix(params: Seq[Double]): Matrix = {
    val phi = params(0)

    Array(
      Array(Complex(1), Complex(0)),
      Array(Complex(0), Complex.e(phi))
    ).toFloat
  }
}