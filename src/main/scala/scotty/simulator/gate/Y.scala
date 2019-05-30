package scotty.simulator.gate

import scotty.quantum.QuantumContext._

object Y {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(Complex(0), Complex(0, -1)),
    Array(Complex(0, 1), Complex(0))
  )
}