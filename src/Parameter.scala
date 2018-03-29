case class Parameter(t: Type, name: Option[String]) {
  def nameString(index: Int): String = {
    name match {
      case Some(n) => n
      case None    => s"param$index"
    }
  }
}
