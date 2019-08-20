package scotty.simulator.gate

import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.math.Complex
import scotty.simulator.math.Implicits._

object Z {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(1, 0, 0, 0),
    Array(0, 0, -1, 0)
  )
}