package scotty.quantum.math

import scotty.quantum.{Bit, Zero}
import scala.annotation.tailrec

object MathUtils {
  val Precision = 1e6

  implicit class DoubleHelpers(d: Double) {
    def toPercent: Double = d * 100

    def ~=(d2: Double): Boolean = {
      if ((d - d2).abs < 1 / Precision) true else false
    }

    def !~=(d2: Double): Boolean = {
      val e = ~=(d2)

      !e
    }
  }

  implicit class IntHelpers(i: Int) {
    def toBinary: Seq[Bit] = toBinaryImpl(i)
  }

  def toBinaryImpl(n: Int): Seq[Bit] = {
    @tailrec
    def binary(acc: Seq[Bit], n: Int): Seq[Bit] = n match {
      case 0 | 1 => Bit(n) +: acc
      case _ => binary(Bit(n % 2) +: acc, n / 2)
    }

    binary(Seq(), n)
  }

  def bitsToInts(bits: Seq[Bit]): List[Int] = bits.map(_.toInt).toList

  def toPaddedBinary(n: Int, qubitCount: Int): List[Bit] = {
    val bits = n.toBinary

    List.fill(qubitCount - bits.length)(Zero()) ++ bits
  }

  def toPaddedBinaryInts(n: Int, qubitCount: Int): List[Int] = bitsToInts(toPaddedBinary(n, qubitCount))

  def toPaddedBinaryString(n: Int, qubitCount: Int): String = toPaddedBinaryInts(n, qubitCount).mkString("")

  def isProbabilityValid(a: Double, b: Double): Boolean = {
    val sumOfSquares = Math.pow(a, 2) + Math.pow(b, 2)

    Math.abs(sumOfSquares - 1) < 1 / MathUtils.Precision
  }
}
