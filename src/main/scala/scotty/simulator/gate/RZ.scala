package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex

object RZ {
  def matrix(params: Seq[Double]): Matrix = {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2), Math.sin(-theta / 2)), Complex(0)),
      Array(Complex(0), Complex(Math.cos(theta / 2), Math.sin(theta / 2)))
    ).toFloat
  }
}