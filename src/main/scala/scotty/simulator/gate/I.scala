package scotty.simulator.gate

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._

case class I(q: Qubit)(implicit val computer: QuantumContext) extends CircuitGate {
  lazy val qs = Seq(q)

  def matrix() = I.matrix
}

object I {
  val matrix = Array(
    Array(Complex(1), Complex(0)),
    Array(Complex(0), Complex(1))
  )
}