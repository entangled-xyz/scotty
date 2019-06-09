package scotty.simulator.math

import scotty.quantum.math.Complex
import org.apache.commons.math3.complex.{Complex => ApacheComplex}

object Implicits {
  implicit def toApacheComplexNestedArray(ca: Array[Array[Complex]]) =
    ca.map(c => c.map(r => new ApacheComplex(r.r, r.i)))
  implicit def toApacheComplexArray(ca: Array[Complex]) = ca.map(c => new ApacheComplex(c.r, c.i))
  implicit def toApacheComplex(c: Complex) = new ApacheComplex(c.r, c.i)

  implicit def toComplexNestedArray(ca: Array[Array[ApacheComplex]]) =
    ca.map(c => c.map(r => Complex(r.getReal, r.getImaginary)))
  implicit def toComplexArray(ca: Array[ApacheComplex]) = ca.map(c => Complex(c.getReal, c.getImaginary))
  implicit def toComplex(c: ApacheComplex) = Complex(c.getReal, c.getImaginary)
}