package scotty.quantum.math

import MathUtils._

case class Complex(r: Double, i: Double = 0) {
  override def toString: String = s"${r.roundWithPrecision} ${if (i >= 0) "+ " else ""}${i.roundWithPrecision}i"

  def abs(): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
}