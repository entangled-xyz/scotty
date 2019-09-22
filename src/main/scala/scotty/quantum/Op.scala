package scotty.quantum

trait Op {
  lazy val qubitCount: Int = indices.length
  val indices: Seq[Int]
}

case class CircuitConnector(circuit: Circuit) extends Op {
  val indices: Range = circuit.indices
}

case class Measure(index: Int) extends Op {
  val indices: Seq[Int] = Seq(index)
}