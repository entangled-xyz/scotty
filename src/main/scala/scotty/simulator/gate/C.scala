package scotty.simulator.gate

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.simulator.StateWithVector
import scotty.simulator.math.{MathUtils, RawGate}

case class C(gateMatrix: Matrix, q1: Qubit, q2: Qubit)(implicit val computer: QuantumContext) extends CircuitGate {
  lazy val qs = Seq(q1, q2)

  def matrix() = C.matrix(qs, gateMatrix)
}

object C {
  def matrix(qs: Seq[Qubit], gateMatrix: Matrix)(implicit computer: QuantumContext): Matrix = {
    def toBasisState(n: Complex): (Complex, Complex) = n match {
      case Complex(1, _) => (Complex(0), Complex(1))
      case Complex(0, _) => (Complex(1), Complex(0))
    }

    def toBinary(n: (Complex, Complex)): Int = if (n == (Complex(0), Complex(1))) 1 else 0

    val indices = qs.map(_.index)
    val gap = Math.abs(indices.reduceLeft(_ - _)) - 1
    val isFlipped = indices.sliding(2).forall { case Seq(x, y) => x > y }

    (0 until Math.pow(2, 2 + gap).toInt).map(index => {
      val rawBinary = MathUtils.toBinary(index).map(Complex(_))
      val binary = (List.fill(2 + gap - rawBinary.length)(Complex(0)) ++ rawBinary).toArray
      val controlIndex = if (isFlipped) binary.length - 1 else 0
      val targetIndex = if (isFlipped) 0 else binary.length - 1

      if (binary(controlIndex) == Complex(1)) {
        val state = StateWithVector(toBasisState(binary(targetIndex)))
        val data = (state applyGate RawGate(gateMatrix)).vector()

        binary(targetIndex) = Complex(toBinary((data(0), data(1))))
      }

      binary
        .map(b => StateWithVector(toBasisState(b)))
        .reduce((s1, s2) => s1 parCombination s2)
        .rawVector
    }).toArray
  }
}

