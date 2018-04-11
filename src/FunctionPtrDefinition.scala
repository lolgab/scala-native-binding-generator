case class FunctionPtrDefinition(t: FunctionType, name: Identifier) extends Definition {
  override def toString: String = s"type $name = $t"
}
