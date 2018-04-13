package bindgen

case class TypeAlias(name: Identifier, content: HasNative) extends Definition {
  override def toString: String = s"type $name = $content"
}