package scotty.simulator.gate

import scotty.quantum.QuantumContext._

object I {
  def matrix(qs: Seq[Qubit], params: Seq[Complex], target: Matrix) = Array(
    Array(Complex(1), Complex(0)),
    Array(Complex(0), Complex(1))
  )
}