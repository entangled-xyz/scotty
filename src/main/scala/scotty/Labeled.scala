package scotty

trait Labeled {
  val label: Option[String]

  def hasLabel(l: String): Boolean = label.isDefined && label.get == l
}
