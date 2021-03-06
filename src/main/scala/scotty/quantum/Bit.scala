package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.math.Complex
import scotty.{ErrorMessage, Labeled}

sealed trait Bit extends Labeled[String] {
  def toComplexArray: Array[Complex] = this match {
    case _: One => Array(Complex(0), Complex(1))
    case _: Zero => Array(Complex(1), Complex(0))
  }

  def toVector: Vector = this match {
    case _: One => Array(0f, 0f, 1f, 0f)
    case _: Zero => Array(1f, 0f, 0f, 0f)
  }

  def toInt: Int = this match {
    case _: One => 1
    case _: Zero => 0
  }

  def withLabel(label: String): Bit = this match {
    case _: One => One(label)
    case _: Zero => Zero(label)
  }

  def toHumanString: String = this match {
    case _: One => label.fold("1")(l => s"One($l)")
    case _: Zero => label.fold("0")(l => s"Zero($l)")
  }
}

case class Zero(label: Option[String]) extends Bit
case class One(label: Option[String]) extends Bit

object Bit {
  def apply(value: Int): Bit = value match {
    case 0 => Zero(None)
    case 1 => One(None)
    case _ => throw new IllegalArgumentException(ErrorMessage.IntToBit)
  }

  def apply(value: Array[Complex]): Bit = value.toSeq match {
    case Seq(Complex(1, 0), Complex(0, 0)) => Zero()
    case Seq(Complex(0, 0), Complex(1, 0)) => One()
    case _ => throw new IllegalArgumentException(ErrorMessage.VectorToBit)
  }
}

object One {
  val floatValue = Array(0f, 0f, 1f, 0f)

  def apply(): One = One(None)
  def apply(label: String): One = One(Some(label))
}

object Zero {
  val floatValue = Array(1f, 0f, 0f, 0f)

  def apply(): Zero = Zero(None)
  def apply(label: String): Zero = Zero(Some(label))
}