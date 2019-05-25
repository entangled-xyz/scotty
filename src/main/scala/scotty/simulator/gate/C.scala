package scotty.simulator.gate

import scotty.math.MathUtils
import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.simulator.SimSuperposition
import scotty.simulator.math.RawGate
import scotty.simulator.math.Implicits._

case class C(gateMatrix: Matrix, q1: Qubit, q2: Qubit)(implicit val computer: QuantumContext) extends CircuitGate {
  lazy val qs = Seq(q1, q2)

  def matrix() = C.matrix(qs, gateMatrix)
}

object C {
  def matrix(qs: Seq[Qubit], gateMatrix: Matrix)(implicit computer: QuantumContext): Matrix = {
    def toBasisState(n: Int): (Complex, Complex) = if (n == 1) (Complex(0), Complex(1)) else (Complex(1), Complex(0))
    def toBinary(a: Complex, b: Complex): Int = if (a == Complex(0) && b == Complex(1)) 1 else 0

    val indices = qs.map(_.index)
    val gap = Math.abs(indices.reduceLeft(_ - _)) - 1
    val isFlipped = indices.sliding(2).forall { case Seq(x, y) => x > y }

    (0 until Math.pow(2, 2 + gap).toInt).map(index => {
      val binary = MathUtils.toBinaryPadded(index, 2 + gap).toArray
      val controlIndex = if (isFlipped) binary.length - 1 else 0
      val targetIndex = if (isFlipped) 0 else binary.length - 1

      if (binary(controlIndex) == 1) {
        val data = RawGate(gateMatrix).product(toBasisState(binary(targetIndex))).getData

        binary(targetIndex) = toBinary(data(0), data(1))
      }

      binary
        .map(b => SimSuperposition(toBasisState(b)))
        .reduce((s1, s2) => s1 parCombination s2)
        .rawVector
    }).toArray
  }
}

