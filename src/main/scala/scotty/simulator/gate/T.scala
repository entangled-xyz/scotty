package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix

object T {
  def matrix(params: Seq[Double]): Matrix = PHASE.matrix(Seq(Math.PI / 4))
}