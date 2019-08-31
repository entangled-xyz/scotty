package scotty.simulator.math

import scotty.quantum.math.Complex

import scala.collection.parallel.immutable.ParVector
import scotty.quantum.QuantumContext.{Matrix, Vector}
import scala.collection.parallel.TaskSupport

/**
  * The <code>Vector</code> object contains pure vector methods from linear algebra.
  *
  * Complex numbers are represented with two consecutive <code>Double</code>s. All vector
  * data is represented in a single array.
  */
object VectorWrapper {
  /**
    * Returns a vector with the tensor product of two input vectors.
    *
    * @param v1 Array of doubles.
    * @param v2 Array of doubles.
    * @return Array of doubles.
    */
  def tensorProduct(v1: Vector, v2: Vector, taskSupport: TaskSupport): Vector = {
    val v1Length = v1.length / 2
    val v2Length = v2.length / 2
    val newData = Array.fill(2 * v1Length * v2Length)(0d)
    val runs = ParVector.iterate(0, v1.length / 2)(i => i + 1)

    runs.tasksupport = taskSupport

    runs.foreach(c1 => {
      for (c2 <- 0 until (v2.length / 2)) {
        val (r, i) = Complex.product(v1(2 * c1), v1(2 * c1 + 1), v2(2 * c2), v2(2 * c2 + 1))

        newData(2 * c1 * v2Length + 2 * c2) = r
        newData(2 * c1 * v2Length + 2 * c2 + 1) = i
      }
    })

    newData
  }

  /**
    * Return the complex conjugate of a vector.
    *
    * @param v Array of doubles.
    * @return Array of doubles.
    */
  def conjugate(v: Vector): Vector = {
    for (s <- 0 until v.length / 2) v(2 * s + 1) = -v(2 * s + 1)
    v
  }

  /**
    * Returns an outer product of the column vector and its column representation.
    *
    * @param v Array of doubles.
    * @return Array of arrays of doubles.
    */
  def ketBraOuterProduct(v: Vector): Matrix = {
    val newData = Array.fill(v.length)(Array.fill(2 * v.length)(0d))

    for (v1Index <- 0 until v.length / 2) {
      for (v2Index <- 0 until v.length / 2) {
        val value = Complex.product(v(2 * v1Index), v(2 * v1Index + 1), v(2 * v2Index), v(2 * v2Index + 1))

        newData(v1Index)(2 * v2Index) = value._1
        newData(v1Index)(2 * v2Index + 1) = value._2
      }
    }

    newData
  }
}