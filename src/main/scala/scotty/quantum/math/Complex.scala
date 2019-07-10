package scotty.quantum.math

import MathUtils._

case class Complex(r: Double, i: Double) {
  override def toString: String = {
    val parsedI = if (i == -0) 0 else i.rounded

    s"${r.rounded}${if (parsedI >= 0) "+" else ""}${parsedI}i"
  }

  def abs: Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
}

object Complex {
  def apply(r: Double): Complex = new Complex(r, 0)
}