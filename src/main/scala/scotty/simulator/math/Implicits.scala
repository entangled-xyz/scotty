package scotty.simulator.math

import org.apache.commons.math3.complex.Complex

object Implicits {
  implicit def toComplexNestedArray(ca: Array[Array[Int]]) = ca.map(c => c.map(i => new Complex(i)))
  implicit def toComplexArray(ca: Array[Int]) = ca.map(i => new Complex(i))

  implicit def toComplexNestedArray(ca: Array[Array[Double]]) = ca.map(c => c.map(d => new Complex(d)))
  implicit def toComplexArray(ca: Array[Double]) = ca.map(d => new Complex(d))
}