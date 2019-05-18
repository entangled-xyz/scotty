package scotty.simulator.math

import scotty.simulator.Config

object MathUtils {
  def round(d: Double): Double = {
    val precision = 1 / Config.PRECISION

    Math.rint(d * precision) / precision
  }
}
