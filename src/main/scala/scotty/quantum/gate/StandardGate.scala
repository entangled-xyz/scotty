package scotty.quantum.gate

import scotty.quantum.gate.TargetGate.MatrixGen
import scotty.quantum.math.Complex

object StandardGate {
  // Single qubit gates

  case class H(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()

    val pc: Float = 1 / Math.sqrt(2).toFloat
    val nc: Float = -1 / Math.sqrt(2).toFloat

    val matrixGen: MatrixGen = _ => Array(
      Array(pc, 0f, pc, 0f),
      Array(pc, 0f, nc, 0f)
    )
  }

  case class I(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = _ => Array(
      Array(1f, 0f, 0f, 0f),
      Array(0f, 0f, 1f, 0f)
    )
  }

  case class X(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = _ => Array(
      Array(0f, 0f, 1f, 0f),
      Array(1f, 0f, 0f, 0f)
    )
  }

  case class Y(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(0), Complex(0, -1)),
      Array(Complex(0, 1), Complex(0))
    ).toFloat
  }

  case class Z(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(1), Complex(0)),
      Array(Complex(0), Complex(-1))
    ).toFloat
  }

  case class S(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = PHASE(Math.PI / 2, index).matrixGen
  }

  case class T(index: Int) extends TargetGate {
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = PHASE(Math.PI / 4, index).matrixGen
  }

  case class PHASE(phi: Double, index: Int) extends TargetGate {
    val params: Seq[Double] = Seq(phi)
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(1), Complex(0)),
      Array(Complex(0), Complex.e(phi))
    ).toFloat
  }

  case class PHASE0(phi: Double, index: Int) extends TargetGate {
    val params: Seq[Double] = Seq(phi)
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex.e(phi), Complex(0)),
      Array(Complex(0), Complex(1))
    ).toFloat
  }

  case class RX(theta: Double, index: Int) extends TargetGate {
    val params: Seq[Double] = Seq(theta)
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toFloat
  }

  case class RY(theta: Double, index: Int) extends TargetGate {
    val params: Seq[Double] = Seq(theta)
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(Math.cos(theta / 2)), Complex(-Math.sin(theta / 2))),
      Array(Complex(Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toFloat
  }

  case class RZ(theta: Double, index: Int) extends TargetGate {
    val params: Seq[Double] = Seq(theta)
    val matrixGen: MatrixGen = _ => Array(
      Array(Complex(Math.cos(theta / 2), Math.sin(-theta / 2)), Complex(0)),
      Array(Complex(0), Complex(Math.cos(theta / 2), Math.sin(theta / 2)))
    ).toFloat
  }

  // Multi qubit gates

  case class CNOT(controlIndex: Int, targetIndex: Int) extends ControlGate {
    lazy val target = X(targetIndex)
  }

  case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends ControlGate {
    lazy val target = Controlled(controlIndex2, X(targetIndex))
  }

  case class CZ(controlIndex: Int, targetIndex: Int) extends ControlGate {
    lazy val target = Z(targetIndex)
  }

  case class SWAP(index1: Int, index2: Int) extends SwapGate

  case class CSWAP(controlIndex: Int, index1: Int, index2: Int) extends ControlGate {
    lazy val target = SWAP(index1, index2)
  }

  case class CPHASE(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate {
    lazy val target = PHASE(phi, targetIndex)
  }

  case class CPHASE10(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate {
    lazy val target = PHASE0(phi, targetIndex)
  }
}
