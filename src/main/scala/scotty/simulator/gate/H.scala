package scotty.simulator.gate

import scotty.quantum.QuantumContext._

object H {
  val pc = Complex(1 / Math.sqrt(2))
  val nc = Complex(-1 / Math.sqrt(2))

  def matrix(qs: Seq[Qubit], params: Seq[Complex], target: Matrix) = Array(
    Array(pc, pc),
    Array(pc, nc)
  )
}