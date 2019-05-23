package scotty.simulator

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext.{Complex, Op}
import scotty.simulator.QuantumSimulator.StateWithVector
import scotty.simulator.math.MathUtils

object Gate {
  val I = Array(
    Array(Complex(1), Complex(0)),
    Array(Complex(0), Complex(1))
  )

  val X = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )

  def C(gate: Op, gap: Int, isFlipped: Boolean)(implicit ctx: QuantumContext): Array[Array[Complex]] = {
    def toBasisState(n: Complex): (Complex, Complex) = n match {
      case Complex(1, _) => (Complex(0), Complex(1))
      case Complex(0, _) => (Complex(1), Complex(0))
    }

    def toBinary(n: (Complex, Complex)): Int = if (n == (Complex(0), Complex(1))) 1 else 0

    (0 until Math.pow(2, 2 + gap).toInt).map(index => {
      val rawBinary = MathUtils.toBinary(index).map(Complex(_))
      val binary = (List.fill(2 + gap - rawBinary.length)(Complex(0)) ++ rawBinary).toArray
      val controlIndex = if (isFlipped) binary.length - 1 else 0
      val targetIndex = if (isFlipped) 0 else binary.length - 1

      if (binary(controlIndex) == Complex(1)) {
        val state = StateWithVector(toBasisState(binary(targetIndex)))
        val data = (state applyOp gate).vector

        binary(targetIndex) = Complex(toBinary((data(0), data(1))))
      }

      binary
        .map(b => StateWithVector(toBasisState(b)))
        .reduce((s1, s2) => s1 combine s2)
        .rawVector
    }).toArray
  }
}
