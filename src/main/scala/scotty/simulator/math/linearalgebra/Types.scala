package scotty.simulator.math.linearalgebra

import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector}
import scotty.quantum.math.Complex.Complex

object Types {
  type ApacheMatrix = Array2DRowFieldMatrix[Complex]
  type ApacheVector = ArrayFieldVector[Complex]
}
