package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object I {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(1, 0, 0, 0),
    Array(0, 0, 1, 0)
  )
}