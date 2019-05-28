package scotty.quantum.math

import scala.annotation.tailrec

object MathUtils {
  val PRECISION = 1e-8

  implicit class DoubleHelpers(d: Double) {
    def roundWithPrecision: Double = {
      val precision = 1 / PRECISION

      Math.rint(d * precision) / precision
    }

    def toPercent: Double = d * 100
  }

  implicit class IntHelpers(i: Int) {
    def toBinary(n: Int): Seq[Int] = {
      @tailrec
      def binary(acc: Seq[Int], n: Int): Seq[Int] = n match {
        case 0 | 1 => n +: acc
        case _ => binary((n % 2) +: acc, n / 2)
      }

      binary(Seq(), n)
    }
  }

  def toBinaryPadded(n: Int, qubitCount: Int): List[Int] = {
    val bits = n.toBinary(n)

    List.fill(qubitCount - bits.length)(0) ++ bits
  }
}
