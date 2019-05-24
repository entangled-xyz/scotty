package scotty.simulator.gate

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._

case class X(q: Qubit)(implicit val computer: QuantumContext) extends CircuitGate {
  lazy val qs = Seq(q)

  def matrix() = X.matrix
}

object X {
  val matrix = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )
}