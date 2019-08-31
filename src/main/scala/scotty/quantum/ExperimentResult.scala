package scotty.quantum

import scotty.quantum.ExperimentResult.StateStats
import scotty.quantum.math.MathUtils

case class ExperimentResult(trials: List[Collapsed]) {
  lazy val stateStats: StateStats = {
    val qs = trials(0).qubitCount

    val state = (0 until Math.pow(2, qs).toInt)
      .map(i => BinaryRegister(MathUtils.toPaddedBinary(i, qs): _*))

    val results = trials
      .map(t => List.fill(Math.pow(2, qs).toInt)(0).updated(t.index, 1))
      .transpose
      .map(_.sum)

    StateStats(state.zip(results).toList)
  }
}

object ExperimentResult {
  case class StateStats(stats: List[(BinaryRegister, Int)]) {
    def toHumanString: String = {
      stats
        .map(s => s"${s._1.values.map(_.toHumanString).mkString("")}: ${s._2}")
        .mkString("\n")
    }
  }
}