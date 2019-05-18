package scotty.simulator.gate

import scotty.quantum.QuantumMachine.{Complex, Qubit, State}
import scotty.simulator.{QuantumSim, SimOp}

object X {
  lazy val data = Array(
    Array(Complex(0), Complex(1)),
    Array(Complex(1), Complex(0))
  )

  def apply(q: Qubit)(implicit sim: QuantumSim): State = sim.applyOp(q)(SimOp(data))
}