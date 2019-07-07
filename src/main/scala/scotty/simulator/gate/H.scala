package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.simulator.math.Implicits._

object H {
  val pc: Double = 1 / Math.sqrt(2)
  val nc: Double = -1 / Math.sqrt(2)

  def matrix(params: Seq[Double]): Matrix = Array(
    Array(pc, pc),
    Array(pc, nc)
  )
}