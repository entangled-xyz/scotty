package scotty.simulator

import scotty.simulator.QuantumSimulator.GateGenAdder
import scotty.simulator.gate._

object GateLoader {
  def loadDefaultGens(implicit sim: QuantumSimulator, adder: GateGenAdder): Unit = {
    adder(sim, "H", H.matrix)
    adder(sim, "X", X.matrix)
    adder(sim, "Y", Y.matrix)
    adder(sim, "Z", Z.matrix)
    adder(sim, "I", I.matrix)
    adder(sim, "RX", RX.matrix)
    adder(sim, "RY", RY.matrix)
    adder(sim, "RZ", RZ.matrix)
  }
}
