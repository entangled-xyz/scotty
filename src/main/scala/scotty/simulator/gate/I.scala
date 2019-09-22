package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object I {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(1f, 0f, 0f, 0f),
    Array(0f, 0f, 1f, 0f)
  )
}