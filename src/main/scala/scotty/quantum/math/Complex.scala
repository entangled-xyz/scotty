package scotty.quantum.math

import MathUtils._

case class Complex(r: Double, i: Double = 0) {
  override def toString: String = s"${r.rounded} ${if (i >= 0) "+ " else ""}${i.rounded}i"

  def abs(): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
}