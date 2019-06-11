package scotty.quantum.math

import scotty.quantum.QuantumContext.QuantumException

import scala.annotation.tailrec

object MathUtils {
  val PRECISION = 1e-8

  implicit class DoubleHelpers(d: Double) {
    def rounded: Double = {
      val precision = 1 / PRECISION

      Math.rint(d * precision) / precision
    }

    def toPercent: Double = d * 100
  }

  implicit class IntHelpers(i: Int) {
    def toBinary: Seq[Int] = toBinaryImpl(i)

    def toBasisState: Array[Complex] =
      if (i == 1) Array(Complex(0), Complex(1))
      else if (i == 0) Array(Complex(1), Complex(0))
      else throw QuantumException("Only classical 1s and 0s can be converted to quantum basis state.")
  }

  implicit class ComplexHelpers(c: Complex) {
    def rounded: Complex = Complex(c.r.rounded, c.i.rounded)
  }

  def toBinaryImpl(n: Int): Seq[Int] = {
    @tailrec
    def binary(acc: Seq[Int], n: Int): Seq[Int] = n match {
      case 0 | 1 => n +: acc
      case _ => binary((n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }

  def toBinaryPadded(n: Int, qubitCount: Int): List[Int] = {
    val bits = n.toBinary

    List.fill(qubitCount - bits.length)(0) ++ bits
  }

  def isProbabilityValid(a: Double, b: Double): Boolean = {
    val sumOfSquares = Math.pow(a, 2) + Math.pow(b, 2)

    Math.abs(sumOfSquares - 1) < MathUtils.PRECISION
  }
}
