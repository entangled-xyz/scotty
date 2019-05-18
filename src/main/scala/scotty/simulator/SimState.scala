package scotty.simulator

import org.apache.commons.math3.complex.ComplexField
import org.apache.commons.math3.linear.ArrayFieldVector
import scotty.quantum.QuantumMachine.{Complex, State}
import org.apache.commons.math3.complex.{Complex => ApacheComplex}
import scotty.simulator.QuantumSim.Implicits._

case class SimState(data: Array[Complex]) extends State {
  lazy val fieldVector = new ArrayFieldVector[ApacheComplex](data, false)

  def ⊗(state: SimState): SimState = kroneckerProduct(state)

  def kroneckerProduct(state: SimState): SimState = {
    if (data.length == 0) {
      state
    } else {
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
  }


  override def toString: String = {
    data.toList.map(c => if (c.i == 0) s"${c.r}" else s"${c.r} + ${c.i}i").toString
  }
}

object SimState {
  def apply(tuple: (Complex, Complex)): SimState = this(Array(tuple._1, tuple._2))
  def apply(): SimState = this(Array[Complex]())
}