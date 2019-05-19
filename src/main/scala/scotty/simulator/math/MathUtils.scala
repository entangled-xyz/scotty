package scotty.simulator.math

import scotty.simulator.Config

import scala.annotation.tailrec

object MathUtils {
  def round(d: Double): Double = {
    val precision = 1 / Config.PRECISION

    Math.rint(d * precision) / precision
  }

  def toBinary(n: Long): Seq[Long] = {
    @tailrec
    def binary(acc: Seq[Long], n: Long): Seq[Long] = n match {
      case 0 | 1 => n +: acc
      case _ => binary((n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }
}
