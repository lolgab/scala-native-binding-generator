case class Enum(name: Identifier, components: Seq[(Identifier, Option[String])])
    extends Definition
    with HasOps {
  def componentsString = {
    var i          = 0
    var lastResult = ""
    components
      .map {
        case (name, optResult) =>
          optResult match {
            case None =>
              val ret =
                if (lastResult == "") s"val $name = $i"
                else s"val $name = ($lastResult) + $i"
              i += 1
              ret
            case Some(result) =>
              val ret = s"val $name = $result"
              i = 0
              ret
          }
      }
      .map(s =>
        name match {
          case Identifier("")    => s"  $s"
          case s => s
      })
      .mkString("\n")
  }

  override def toString = ""

  def commonSubString: String = {
    val names = components.map(_._1.toString)

    names.foldLeft("")((_, _) =>
      (names.min, names.max).zipped.takeWhile(v => v._1 == v._2).unzip._1.mkString)
  }

  def opsString =
    name match {
      case Identifier("") =>
        val sub = commonSubString
        if (sub.length > 2) sub else s"Enum${hashCode()}"
          s"""object $sub {
             |$componentsString
             |}""".stripMargin
      case n =>
        s"""object $n {
           |$componentsString
           |}""".stripMargin
    }
}
