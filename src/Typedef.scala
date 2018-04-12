case class Typedef(name: Identifier, definition: HasNative) extends Definition {
  override def toString: String = {
    s"""type $name = $definition"""
  }
}