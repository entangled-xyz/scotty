package scotty

import org.scalactic.{Equality, TolerantNumerics}
import scotty.quantum.math.Complex
import scotty.simulator.QuantumSimulator

trait TestHelpers {
  implicit val sim = QuantumSimulator()
  val quarterTurn = Math.PI / 2
  val thirdTurn = 2 * Math.PI / 3
  val fiftyPercent = Math.sqrt(2) / 2
  val Precision = 1e8
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
  implicit val floatEquality = TolerantNumerics.tolerantFloatEquality(0.01f)
  implicit val complexEquality = tolerantComplexEquality(0.01f)

  def tolerantComplexEquality(tolerance: Float): Equality[Complex] = {
    if (tolerance <= 0.0)
      throw new IllegalArgumentException(tolerance.toString +
        " passed to tolerantComplexEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Complex] {
      def areEqual(a: Complex, b: Any): Boolean = {
        b match {
          case bComplex: Complex =>
            (a.r <= bComplex.r + tolerance) &&
              (a.r >= bComplex.r - tolerance) &&
              (a.i <= bComplex.i + tolerance) &&
              (a.i >= bComplex.i - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantComplexEquality($tolerance)"
    }
  }
}
