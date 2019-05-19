package scotty.simulator.gate

import scotty.quantum.QuantumMachine.Complex
import scotty.simulator.{QuantumSim, SimOp, SimQubit, SimState}
import scotty.simulator.math.MathUtils

object C {
  def apply(gate: Array[Array[Complex]], gap: Int)(implicit sim: QuantumSim): Array[Array[Complex]] = {
    (0 until Math.pow(2, 2 + gap).toInt).map(index => {
      val binary = MathUtils.toBinary(index).map(Complex(_))
      val paddedBinary = (List.fill(2 + gap - binary.length)(Complex(0)) ++ binary).toArray

      if (paddedBinary(0) == Complex(1)) {
        val lastIndex = paddedBinary.length - 1
        val data = (SimOp(gate) * SimState(toBasisState(paddedBinary(lastIndex)))).data

        paddedBinary(lastIndex) = SimQubit.from((data(0), data(1)))
      }

      paddedBinary
    }).toArray
  }

  def toBasisState(n: Complex): (Complex, Complex) = n match {
    case Complex(1, _) => (Complex(0), Complex(1))
    case Complex(0, _) => (Complex(1), Complex(0))
  }
}
