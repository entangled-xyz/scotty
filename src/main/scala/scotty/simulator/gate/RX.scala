package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex

object RX {
  def matrix(params: Seq[Double]): Matrix = {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toFloat
  }
}