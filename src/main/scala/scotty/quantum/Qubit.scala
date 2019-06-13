package scotty.quantum

import scotty.{ErrorMessage, Labeled}
import scotty.quantum.math.{Complex, MathUtils}

case class Qubit(a: Complex, b: Complex, label: Option[String]) extends Labeled[String] {
  require(Qubit.areAmplitudesValid(this), ErrorMessage.QubitAmplitudesError)
}

object Qubit {
  def one(label: Option[String]): Qubit = Qubit(Complex(0), Complex(1), label)

  def one: Qubit = one(None)

  def zero(label: Option[String]): Qubit = Qubit(Complex(1), Complex(0), label)

  def zero: Qubit = zero(None)

  def fiftyFifty(label: Option[String]): Qubit = this(Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)))

  def fiftyFifty: Qubit = fiftyFifty(None)

  def areAmplitudesValid(q: Qubit): Boolean = MathUtils.isProbabilityValid(q.a.abs, q.b.abs)

  def apply(as: Array[Complex]): Qubit = this(as(0), as(1))

  def apply(a: Complex, b: Complex): Qubit = this(a, b, None)
}