package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object X {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(0, 0, 1, 0),
    Array(1, 0, 0, 0)
  )
}