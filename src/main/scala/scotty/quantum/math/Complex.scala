package scotty.quantum.math

import scotty.quantum.QuantumContext.{Matrix, Vector}

case class Complex(r: Float, i: Float) {
  override def toString: String = Complex.toString(this)
}

object Complex {
  implicit class ComplexToFloatVector(vs: Array[Complex]) {
    def toFloat: Vector = vs.map(v => Array(v.r, v.i)).flatten
  }

  implicit class ComplexToFloatMatrix(vs: Array[Array[Complex]]) {
    def toFloat: Matrix = vs.map(r => r.map(v => Array(v.r, v.i)).flatten)
  }

  implicit class ComplexToFloatVectorList(vs: List[Complex]) {
    def toFloat: List[Float] = vs.flatMap(v => Array(v.r, v.i))
  }

  implicit class ComplexToFloatMatrixList(vs: List[List[Complex]]) {
    def toFloat: List[List[Float]] = vs.map(r => r.flatMap(v => Array(v.r, v.i)))
  }

  def apply(r: Float): Complex = Complex(r, 0)

  def apply(r: Double): Complex = Complex(r.toFloat, 0)

  def apply(r: Double, i: Double): Complex = Complex(r.toFloat, i.toFloat)

  def product(r1: Float, i1: Float, r2: Float, i2: Float): (Float, Float) = (
    r1 * r2 - i1 * i2,
    r1 * i2 + i1 * r2
  )

  def sum(c1: (Float, Float), c2: (Float, Float)): (Float, Float) = sum(c1._1, c1._2, c2._1, c2._2)

  def sum(r1: Float, i1: Float, r2: Float, i2: Float): (Float, Float) = (
    r1 +r2,
    i1 + i2
  )

  def e(phi: Double): Complex = Complex(Math.cos(phi), Math.sin(phi))

  def abs(r: Float, i: Float): Float = Math.sqrt(Math.pow(r, 2) + Math.pow(i, 2)).toFloat

  def abs(value: Complex): Float = abs(value.r, value.i)

  def toString(c: Complex): String = {
    val r = c.r
    val i = if (c.i == -0) 0 else c.i

    f"$r%1.3f${if (i >= 0) "+" else ""}$i%1.3fi"
  }
}
