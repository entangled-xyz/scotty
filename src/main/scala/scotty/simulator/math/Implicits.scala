package scotty.simulator.math

import scotty.quantum.math.Complex

object Implicits {
  implicit def toComplexNestedArray(ca: Array[Array[Int]]) = ca.map(c => c.map(i => Complex(i)))
  implicit def toComplexArray(ca: Array[Int]) = ca.map(i => Complex(i))

  implicit def toComplexNestedArray(ca: Array[Array[Double]]) = ca.map(c => c.map(d => Complex(d)))
  implicit def toComplexArray(ca: Array[Double]) = ca.map(d => Complex(d))
}