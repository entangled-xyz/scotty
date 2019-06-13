package scotty

trait Labeled[T] {
  val label: Option[T]

  def hasLabel(l: T): Boolean = label.isDefined && label.get == l
}
