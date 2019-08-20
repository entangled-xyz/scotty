package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object H {
  val pc: Double = 1 / Math.sqrt(2)
  val nc: Double = -1 / Math.sqrt(2)

  def matrix(params: Seq[Double]): Matrix = Array(
    Array(pc, 0, pc, 0),
    Array(pc, 0, nc, 0)
  )
}