package scotty.simulator

import org.apache.commons.math3.complex.ComplexField
import org.apache.commons.math3.linear.ArrayFieldVector
import scotty.quantum.QuantumComputer.{Complex, State}
import org.apache.commons.math3.complex.{Complex => ApacheComplex}
import scotty.simulator.math.Implicits._

case class SimState(vector: Array[Complex]) extends State {
  lazy val fieldVector = new ArrayFieldVector[ApacheComplex](vector, false)

  def map(f: ApacheComplex => ApacheComplex): SimState = {
    val resultVector = new ArrayFieldVector(ComplexField.getInstance, fieldVector.getDimension)

    for (index <- 0 until fieldVector.getDimension) {
      resultVector.setEntry(index, f(fieldVector.getEntry(index)))
    }

    SimState(resultVector.getData)
  }

  def ⊗(state: SimState): SimState = kroneckerProduct(state)

  def *(factor: Complex): SimState = scalarProduct(factor)

  def scalarProduct(factor: Complex): SimState = map(entry => entry.multiply(factor))

  def kroneckerProduct(state: SimState): SimState = {
    val v1Size = fieldVector.getDimension
    val v2Size = state.fieldVector.getDimension
    val resultVector = new ArrayFieldVector(ComplexField.getInstance, v1Size * v2Size)

    for (v1Index <- 0 until v1Size) {
      for (v2Index <- 0 until v2Size) {
        resultVector.setEntry(
          (v1Index * v2Size) + v2Index,
          fieldVector.getEntry(v1Index).multiply(state.fieldVector.getEntry(v2Index))
        )
      }
    }

    SimState(resultVector.getData)
  }


  override def toString: String = {
    vector.toList.map(c => if (c.i == 0) s"${c.r}" else s"${c.r} + ${c.i}i").toString
  }
}

object SimState {
  def apply(tuple: (Complex, Complex)): SimState = this(Array(tuple._1, tuple._2))
  def apply(): SimState = this(Array[Complex]())
}