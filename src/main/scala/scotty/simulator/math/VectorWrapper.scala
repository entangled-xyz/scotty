package scotty.simulator.math

import scotty.quantum.math.Complex

import scala.collection.parallel.immutable.ParVector
import scotty.quantum.QuantumContext.{Matrix, Vector}

/**
  * The <code>Vector</code> class contains a representation of a complex number vector.
  * Complex numbers are represented with two consecutive <code>Double</code>s. All vector
  * data is represented in a single array.
  *
  * @param data Raw data representing all complex numbers in a flat array of <code>Double</code>s.
  */
case class VectorWrapper(data: Vector) {
  def size: Int = data.length / 2

  def tensorProduct(vector: VectorWrapper): VectorWrapper =
    VectorWrapper(VectorWrapper.tensorProduct(data, vector.data))
}

object VectorWrapper {
  def tensorProduct(v1: Vector, v2: Vector): Vector = {
    val v1Length = v1.length / 2
    val v2Length = v2.length / 2
    val newData = Array.fill(2 * v1Length * v2Length)(0d)

    ParVector.iterate(0, v1.length / 2)(i => i + 1).foreach(c1 => {
      for (c2 <- 0 until (v2.length / 2)) {
        val (r, i) = Complex.product(v1(2 * c1), v1(2 * c1 + 1), v2(2 * c2), v2(2 * c2 + 1))

        newData(2 * c1 * v2Length + 2 * c2) = r
        newData(2 * c1 * v2Length + 2 * c2 + 1) = i
      }
    })

    newData
  }

  def conjugate(v: Vector): Vector = {
    for (s <- 0 until v.length / 2) {
      v(2 * s + 1) = -v(2 * s + 1)
    }

    v
  }

  // TODO: implement
  def outerProduct(v1: Vector, v2: Vector): Matrix = {
    val newData = Array.fill(v1.length)(Array.fill(2 * v1.length)(0d))

    newData
  }
}