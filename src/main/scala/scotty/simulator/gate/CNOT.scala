package scotty.simulator.gate

import scotty.quantum.QuantumContext
import scotty.quantum.QuantumContext._
import scotty.quantum.math.MathUtils
import scotty.simulator.SimSuperposition
import scotty.simulator.math.Implicits._
import scotty.simulator.math.RawGate

object CNOT {
  def matrix(qs: Seq[Qubit], params: Seq[Complex], target: Matrix)(implicit computer: QuantumContext): Matrix = {
    C.matrix(qs, params, target)
  }
}

