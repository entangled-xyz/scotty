package scotty.simulator.gate

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._

case class H(q: Qubit)(implicit val computer: QuantumContext) extends CircuitGate {
  lazy val qs = Seq(q)

  def matrix() = H.matrix
}

object H {
  val pc = Complex(1 / Math.sqrt(2))
  val nc = Complex(-1 / Math.sqrt(2))

  val matrix = Array(
    Array(pc, pc),
    Array(pc, nc)
  )
}