case class NameAlias(oldN: String, newN: String) extends Definition {
  def toNative: String = s"type $newN = $oldN"
}
