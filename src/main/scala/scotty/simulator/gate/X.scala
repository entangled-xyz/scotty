package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.quantum.math.Complex

object X {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )
}