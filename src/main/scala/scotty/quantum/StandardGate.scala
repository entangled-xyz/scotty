package scotty.quantum

object StandardGate {
  case class Controlled(controlIndex: Int, target: Gate) extends Control

  case class H(index1: Int) extends Target

  case class I(index1: Int) extends Target

  case class X(index1: Int) extends Target

  case class Y(index1: Int) extends Target

  case class Z(index1: Int) extends Target

  case class RX(theta: Double, index1: Int) extends Target {
    override val params: Seq[Double] = Seq(theta)
  }

  case class RY(theta: Double, index1: Int) extends Target {
    override val params: Seq[Double] = Seq(theta)
  }

  case class RZ(theta: Double, index1: Int) extends Target {
    override val params: Seq[Double] = Seq(theta)
  }

  case class CNOT(controlIndex: Int, targetIndex: Int) extends Control {
    lazy val target = X(targetIndex)
  }

  case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends Control {
    lazy val target = Controlled(controlIndex2, X(targetIndex))
  }

  case class SWAP(index1: Int, index2: Int) extends QubitSwap
}
