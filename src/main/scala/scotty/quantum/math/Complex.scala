package scotty.quantum.math

import scotty.quantum.QuantumContext.{Matrix, Vector}

case class Complex(r: Double, i: Double)

object Complex {
  implicit class ComplexToDoubleVector(vs: Array[Complex]) {
    def toDouble: Vector = vs.map(v => Array(v.r, v.i)).flatten
  }

  implicit class ComplexToDoubleMatrix(vs: Array[Array[Complex]]) {
    def toDouble: Matrix = vs.map(r => r.map(v => Array(v.r, v.i)).flatten)
  }

  implicit class ComplexToDoubleVectorList(vs: List[Complex]) {
    def toDouble: List[Double] = vs.flatMap(v => Array(v.r, v.i))
  }

  implicit class ComplexToDoubleMatrixList(vs: List[List[Complex]]) {
    def toDouble: List[List[Double]] = vs.map(r => r.flatMap(v => Array(v.r, v.i)))
  }

  def apply(r: Double): Complex = Complex(r, 0)

  def product(r1: Double, i1: Double, r2: Double, i2: Double): (Double, Double) = (
    r1 * r2 - i1 * i2,
    r1 * i2 + i1 * r2
  )

  def sum(r1: Double, i1: Double, r2: Double, i2: Double): (Double, Double) = (
    r1 +r2,
    i1 + i2
  )

  def e(phi: Double): Complex = Complex(Math.cos(phi), Math.sin(phi))

  def abs(r: Double, i: Double): Double = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2))

  def abs(value: Complex): Double = abs(value.r, value.i)

  def toString(c: Complex): String = {
    val r = c.r
    val i = if (c.i == -0) 0 else c.i

    f"$r%1.3f${if (i >= 0) "+" else ""}$i%1.3fi"
  }
}
