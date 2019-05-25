package scotty.simulator.gate

import scotty.quantum.QuantumContext._

object X {
  def matrix(qs: Seq[Qubit], params: Seq[Complex], target: Matrix) = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )
}