package scotty.simulator.gate

import scotty.quantum.QuantumMachine.{Complex, Qubit, State}
import scotty.simulator.{QuantumSim, SimOp}

object I {
  lazy val data = Array(
    Array(Complex(1), Complex(0)),
    Array(Complex(0), Complex(1))
  )

  def apply(q: Qubit)(implicit sim: QuantumSim): State = sim.applyOp(q)(SimOp(data))
}