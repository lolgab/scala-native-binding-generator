case class NameAlias(name: Identifier, newName: Identifier) extends Definition {
  override def toString: String = s"type $newName = $name"
}
