package scotty.quantum

trait Labeled {
  val label: Option[String]

  def hasLabel(l: String): Boolean = label.contains(l)
}
