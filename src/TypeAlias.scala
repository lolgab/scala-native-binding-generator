case class TypeAlias(tpe: VariableType, name: String) extends Definition {
  override def toNative: String = s"type $name = ${tpe.toNative}"
}
