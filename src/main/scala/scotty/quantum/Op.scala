package scotty.quantum

trait Op {
  lazy val qubitCount: Int = indexes.length
  val indexes: Seq[Int]
}

case class CircuitConnector(circuit: Circuit) extends Op {
  val indexes: Range = circuit.indexes
}

case class Measure(index: Int) extends Op {
  val indexes: Seq[Int] = Seq(index)
}