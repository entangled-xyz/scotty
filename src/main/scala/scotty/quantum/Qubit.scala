package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.{Complex, MathUtils}
import scotty.{ErrorMessage, Labeled}

case class Qubit(a: Complex, b: Complex, label: Option[String]) extends Labeled[String] {
  require(Qubit.areAmplitudesValid(this), ErrorMessage.IncorrectQubitAmplitudes)

  def toVector: Vector = Array(a.r, a.i, b.r, b.i)

  def toHumanString: String = s"Qubit(${a.toString}, ${b.toString})"

  def probabilityOfZero: Double = Math.pow(Complex.abs(a), 2)

  def probabilityOfOne: Double = Math.pow(Complex.abs(b), 2)
}

object Qubit {
  def zero(label: Option[String]): Qubit = Qubit(Complex(1), Complex(0), label)

  def zero(label: String): Qubit = zero(Some(label))

  def zero: Qubit = zero(None)
  
  def one(label: Option[String]): Qubit = Qubit(Complex(0), Complex(1), label)

  def one(label: String): Qubit = one(Some(label))

  def one: Qubit = one(None)

  def fiftyFifty(label: Option[String]): Qubit = this(Complex(1 / Math.sqrt(2.0)), Complex(1 / Math.sqrt(2.0)), label)

  def fiftyFifty(label: String): Qubit = fiftyFifty(Some(label))

  def fiftyFifty: Qubit = fiftyFifty(None)

  def areAmplitudesValid(q: Qubit): Boolean = MathUtils.isProbabilityValid(Complex.abs(q.a), Complex.abs(q.b))

  def apply(as: Array[Complex]): Qubit = this(as(0), as(1))

  def apply(a: Complex, b: Complex): Qubit = this(a, b, None)

  def apply(bit: Bit): Qubit = bit match {
    case _: Zero => Qubit.zero
    case _: One => Qubit.one
  }

  def apply(a: Complex, b: Complex, label: String): Qubit = this(a, b, Some(label))
}