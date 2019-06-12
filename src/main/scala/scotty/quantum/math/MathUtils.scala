package scotty.quantum.math

import scotty.quantum.{Bit, Zero}

import scala.annotation.tailrec

object MathUtils {
  val Precision = 1e-8

  implicit class DoubleHelpers(d: Double) {
    def rounded: Double = {
      val precision = 1 / Precision

      Math.rint(d * precision) / precision
    }

    def toPercent: Double = d * 100
  }

  implicit class IntHelpers(i: Int) {
    def toBinary: Seq[Bit] = toBinaryImpl(i)
  }

  implicit class ComplexHelpers(c: Complex) {
    def rounded: Complex = Complex(c.r.rounded, c.i.rounded)
  }

  def toBinaryImpl(n: Int): Seq[Bit] = {
    @tailrec
    def binary(acc: Seq[Bit], n: Int): Seq[Bit] = n match {
      case 0 | 1 => Bit.fromInt(n) +: acc
      case _ => binary(Bit.fromInt(n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }

  def toBinaryPadded(n: Int, qubitCount: Int): List[Bit] = {
    val bits = n.toBinary

    List.fill(qubitCount - bits.length)(Zero) ++ bits
  }

  def isProbabilityValid(a: Double, b: Double): Boolean = {
    val sumOfSquares = Math.pow(a, 2) + Math.pow(b, 2)

    Math.abs(sumOfSquares - 1) < MathUtils.Precision
  }
}
