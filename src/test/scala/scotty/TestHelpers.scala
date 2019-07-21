package scotty

import org.scalactic.{Equality, TolerantNumerics}
import scotty.quantum.math.Complex.Complex
import scotty.simulator.QuantumSimulator

trait TestHelpers {
  val sim = QuantumSimulator()
  val quarterTurn = Math.PI / 2
  val fiftyPercent = Math.sqrt(2) / 2
  val Precision = 1e8
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
  implicit val complexEquality = tolerantComplexEquality(0.01)

  def tolerantComplexEquality(tolerance: Double): Equality[Complex] = {
    if (tolerance <= 0.0)
      throw new IllegalArgumentException(tolerance.toString +
        " passed to tolerantComplexEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Complex] {
      def areEqual(a: Complex, b: Any): Boolean = {
        b match {
          case bComplex: Complex =>
            (a.getReal <= bComplex.getReal + tolerance) &&
              (a.getReal >= bComplex.getReal - tolerance) &&
              (a.getImaginary <= bComplex.getImaginary + tolerance) &&
              (a.getImaginary >= bComplex.getImaginary - tolerance)
          case _ => false
        }
      }
      override def toString: String = s"TolerantComplexEquality($tolerance)"
    }
  }
}
