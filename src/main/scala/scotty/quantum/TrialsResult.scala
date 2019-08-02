package scotty.quantum

case class TrialsResult(trials: List[Collapsed]) {
  lazy val stateStats: List[Int] = trials
    .map(t => List.fill(Math.pow(2, t.qubitCount).toInt)(0).updated(t.index, 1))
    .transpose
    .map(_.sum)
}