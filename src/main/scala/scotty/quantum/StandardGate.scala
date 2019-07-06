package scotty.quantum

object StandardGate {
  case class Controlled(controlIndex: Int, target: Gate) extends ControlGate

  case class H(index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
  }

  case class I(index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
  }

  case class X(index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
  }

  case class Y(index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
  }

  case class Z(index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
  }

  case class RX(theta: Double, index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
    override val params: Seq[Double] = Seq(theta)
  }

  case class RY(theta: Double, index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
    override val params: Seq[Double] = Seq(theta)
  }

  case class RZ(theta: Double, index: Int) extends TargetGate {
    val indexes: Seq[Int] = Seq(index)
    override val params: Seq[Double] = Seq(theta)
  }

  case class CNOT(controlIndex: Int, targetIndex: Int) extends ControlGate {
    lazy val target = X(targetIndex)
  }

  case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends ControlGate {
    lazy val target = Controlled(controlIndex2, X(targetIndex))
  }

  case class SWAP(index1: Int, index2: Int) extends SwapGate
}
