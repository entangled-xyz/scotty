package scotty.quantum.math

case class Complex(r: Double, i: Double = 0) {
  override def toString: String = s"$r ${if (i >= 0) "+ " else ""}${i}i"

  def abs(): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))
}