package scotty.quantum

object StandardGate {
  case class Controlled(controlIndex: Int, target: Gate) extends Control

  case class H(index: Int) extends Target

  case class I(index: Int) extends Target

  case class X(index: Int) extends Target

  case class Y(index: Int) extends Target

  case class Z(index: Int) extends Target

  case class RX(theta: Double, index: Int) extends Target {
    override val params = Seq(theta)
  }

  case class RY(theta: Double, index: Int) extends Target {
    override val params = Seq(theta)
  }

  case class RZ(theta: Double, index: Int) extends Target {
    override val params = Seq(theta)
  }

  case class CNOT(controlIndex: Int, targetIndex: Int) extends Control {
    val target = X(targetIndex)
  }

  case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends Control {
    val target = Controlled(controlIndex2, X(targetIndex))
  }
}
