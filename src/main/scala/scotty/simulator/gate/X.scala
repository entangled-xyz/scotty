package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.simulator.math.Implicits._

object X {
  def matrix(params: Seq[Double]): Matrix = Array(
    Array(0, 1),
    Array(1, 0)
  )
}