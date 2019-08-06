package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.Complex.Complex
import scotty.quantum.math.{Complex, MathUtils}
import scotty.{ErrorMessage, Labeled}

case class Qubit(a: Complex, b: Complex, label: Option[String]) extends Labeled[String] {
  require(Qubit.areAmplitudesValid(this), ErrorMessage.IncorrectQubitAmplitudes)

  def toBasisState: Vector = Array(a, b)

  def toHumanString: String = s"Qubit(${Complex.toString(a)}, ${Complex.toString(b)})"

  def probabilityOfZero: Double = Math.pow(a.abs, 2)

  def probabilityOfOne: Double = Math.pow(b.abs, 2)
}

object Qubit {
  def one(label: String): Qubit = Qubit(Complex(0), Complex(1), Some(label))

  def one: Qubit = Qubit(Complex(0), Complex(1), None)

  def zero(label: String): Qubit = Qubit(Complex(1), Complex(0), Some(label))

  def zero: Qubit = Qubit(Complex(1), Complex(0), None)

  def fiftyFifty(label: String): Qubit = this(Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)), Some(label))

  def fiftyFifty: Qubit = this(Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)), None)

  def areAmplitudesValid(q: Qubit): Boolean = MathUtils.isProbabilityValid(q.a.abs, q.b.abs)

  def apply(as: Array[Complex]): Qubit = this(as(0), as(1))

  def apply(a: Complex, b: Complex): Qubit = this(a, b, None)

  def apply(a: Complex, b: Complex, label: String): Qubit = this(a, b, Some(label))
}