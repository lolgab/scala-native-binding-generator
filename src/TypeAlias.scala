case class TypeAlias(tpe: VariableType, name: Identifier) extends Definition {
  override def toString: String = s"type $name = $tpe"
}
