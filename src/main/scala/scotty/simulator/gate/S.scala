package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.quantum.math.Complex

object S {
  def matrix(params: Seq[Double]): Matrix = PHASE.matrix(Seq(Math.PI / 2))
}