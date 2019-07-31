package scotty.quantum.math

import org.apache.commons.math3.complex.{Complex => ApacheComplex}

object Complex {
  type Complex = ApacheComplex

  def apply(r: Double): ApacheComplex = new ApacheComplex(r, 0)

  def apply(r: Double, i: Double): ApacheComplex = new ApacheComplex(r, i)

  def toString(c: Complex): String = {
    val r = c.getReal
    val i = if (c.getImaginary == -0) 0 else c.getImaginary

    f"$r%1.3f${if (i >= 0) "+" else ""}$i%1.3fi"
  }

  def e(phi: Double): Complex = Complex(Math.cos(phi), Math.sin(phi))
}