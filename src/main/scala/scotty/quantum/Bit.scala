package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.math.Complex

sealed trait Bit {
  def toBasisState: Array[Complex] = this match {
    case One => Array(Complex(0), Complex(1))
    case Zero => Array(Complex(1), Complex(0))
  }
}

case object Zero extends Bit
case object One extends Bit

object Bit {
  def fromInt(value: Int): Bit = value match {
    case 0 => Zero
    case 1 => One
    case _ => throw new IllegalArgumentException(ErrorMessage.IntToBitError)
  }
}