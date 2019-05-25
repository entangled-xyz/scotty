package scotty.simulator

import scotty.simulator.gate._

object GateLoader {
  def loadDefaultGens(implicit sim: QuantumSimulator): Unit = {
    sim.addGateGen("H", H.matrix)
    sim.addGateGen("X", X.matrix)
    sim.addGateGen("I", I.matrix)
    sim.addGateGen("C", C.matrix)
    sim.addGateGen("CNOT", CNOT.matrix)
  }
}
