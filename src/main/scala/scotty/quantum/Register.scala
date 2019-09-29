package scotty.quantum

import scotty.{ErrorMessage, Labeled}

sealed trait Register[T <: Labeled[String]] {
  val values: Seq[T]

  def size: Int = values.length

  def areLabelsUnique: Boolean = {
    val labels = values.flatMap(v => v.label)

    labels.distinct.size == labels.size
  }

  require(areLabelsUnique, ErrorMessage.RegisterLabelsNotUnique)
}

case class QubitRegister(values: Qubit*) extends Register[Qubit]

case class BitRegister(values: Bit*) extends Register[Bit]