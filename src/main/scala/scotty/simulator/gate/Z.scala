package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex

object Z {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(Complex(1), Complex(0)),
    Array(Complex(0), Complex(-1))
  ).toFloat
}