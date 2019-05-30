package scotty.simulator.gate

import scotty.quantum.QuantumContext._
import scotty.quantum.math.Complex

object H {
  val pc = Complex(1 / Math.sqrt(2))
  val nc = Complex(-1 / Math.sqrt(2))

  def matrix(params: Seq[Double]): Matrix = Array(
    Array(pc, pc),
    Array(pc, nc)
  )
}