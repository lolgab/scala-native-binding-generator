case class ExternTypeAlias(name: Identifier) extends Definition {
  override def toString: String = s"type $name = extern"
}
