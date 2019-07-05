package scotty.simulator.math.linearalgebra

import org.apache.commons.math3.complex.{ComplexField, Complex => ApacheComplex}
import org.apache.commons.math3.linear.ArrayFieldVector
import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.Complex
import scotty.simulator.math.Implicits._
import scotty.simulator.math.LinearAlgebra.ApacheVector

case class VectorWrapper(vector: Array[Complex]) {
  lazy val fieldVector: ArrayFieldVector[ApacheComplex] = VectorWrapper.fieldVector(vector)

  def map(f: ApacheComplex => ApacheComplex): ApacheVector = {
    val resultVector = new ArrayFieldVector(ComplexField.getInstance, fieldVector.getDimension)

    for (index <- 0 until fieldVector.getDimension) {
      resultVector.setEntry(index, f(fieldVector.getEntry(index)))
    }

    resultVector
  }

  def ⊗(v: ApacheVector): ApacheVector = tensorProduct(v)

  def *(factor: Complex): ApacheVector = scalarProduct(factor)

  def scalarProduct(factor: Complex): ApacheVector = map(entry => entry.multiply(factor))

  def tensorProduct(v: ApacheVector): ApacheVector = {
    val v1Size = fieldVector.getDimension
    val v2Size = v.getDimension
    val resultVector = new ArrayFieldVector(ComplexField.getInstance, v1Size * v2Size)

    for (v1Index <- 0 until v1Size) {
      for (v2Index <- 0 until v2Size) {
        resultVector.setEntry(
          (v1Index * v2Size) + v2Index,
          fieldVector.getEntry(v1Index).multiply(v.getEntry(v2Index))
        )
      }
    }

    resultVector
  }
}

object VectorWrapper {
  def fieldVector(vector: Vector) = new ArrayFieldVector[ApacheComplex](vector, false)
}