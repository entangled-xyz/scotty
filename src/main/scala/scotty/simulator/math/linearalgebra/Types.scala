package scotty.simulator.math.linearalgebra

import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector}

object Types {
  type ApacheMatrix = Array2DRowFieldMatrix[Complex]
  type ApacheVector = ArrayFieldVector[Complex]
}
