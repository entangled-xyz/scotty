package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.simulator.math.Implicits._

object Z {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(1, 0),
    Array(0, -1)
  )
}