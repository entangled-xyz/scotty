package scotty.quantum.math

case class Complex(r: Double, i: Double) {
  def abs: Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))

  def *(c: Complex): Complex = Complex(r * c.r - i * c.i, r * c.i + i * c.r)

  def +(c: Complex): Complex = Complex(r + c.r, i + c.i)
}

object Complex {
  def apply(r: Double): Complex = Complex(r, 0)

  def product(r1: Double, i1: Double, r2: Double, i2: Double): (Double, Double) = {
    (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)
  }

  def e(phi: Double): Complex = Complex(Math.cos(phi), Math.sin(phi))

  def toString(c: Complex): String = {
    val r = c.r
    val i = if (c.i == -0) 0 else c.i

    f"$r%1.3f${if (i >= 0) "+" else ""}$i%1.3fi"
  }
}
