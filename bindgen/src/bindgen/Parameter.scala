package bindgen

case class Parameter(t: Type, name: Option[Identifier]) {
  def nameIdentifier(index: Int): Identifier = {
    name match {
      case Some(n) => n
      case None    => Identifier(s"param$index")
    }
  }
}
