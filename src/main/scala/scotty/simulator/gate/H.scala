package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object H {
  val pc: Float = 1 / Math.sqrt(2).toFloat
  val nc: Float = -1 / Math.sqrt(2).toFloat

  def matrix(params: Seq[Double]): Matrix = Array(
    Array(pc, 0f, pc, 0f),
    Array(pc, 0f, nc, 0f)
  )
}