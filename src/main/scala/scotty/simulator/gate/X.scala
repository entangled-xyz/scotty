package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object X {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(0f, 0f, 1f, 0f),
    Array(1f, 0f, 0f, 0f)
  )
}