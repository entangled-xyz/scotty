package scotty.quantum.math

import scala.annotation.tailrec

object MathUtils {
  val PRECISION = 1e-8

  def round(d: Double): Double = {
    val precision = 1 / PRECISION

    Math.rint(d * precision) / precision
  }

  def toBinary(n: Int): Seq[Int] = {
    @tailrec
    def binary(acc: Seq[Int], n: Int): Seq[Int] = n match {
      case 0 | 1 => n +: acc
      case _ => binary((n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }

  def toBinaryPadded(n: Int, qubitCount: Int): List[Int] = {
    val bits = toBinary(n)

    List.fill(qubitCount - bits.length)(0) ++ bits
  }
}
