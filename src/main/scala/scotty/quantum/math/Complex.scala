package scotty.quantum.math

import org.apache.commons.math3.complex.{Complex => ApacheComplex}

object Complex {
  type Complex = ApacheComplex

  def apply(r: Double): ApacheComplex = new ApacheComplex(r, 0)

  def apply(r: Double, i: Double): ApacheComplex = new ApacheComplex(r, i)
}