case class Enum(name: String, components: Seq[(String, Option[String])]) extends Definition with HasOps {
  def componentsString = {
    var i = 0
    var lastResult = ""
    components
      .map { case (name, optResult) => optResult match {
        case None =>
          val ret =
            if(lastResult == "") s"val $name = $i"
            else s"val $name = ($lastResult) + $i"
          i += 1
          ret
        case Some(result) =>
          val ret = s"val $name = $result"
          i = 0
          ret
      }}
      .map(s => if (name == "") s"  $s" else s)
      .mkString("\n")
  }

  def toNative = ""

  def toNativeOps =
    if (name == "")
      componentsString
    else
      s"""object $name {
         |$componentsString
         |}""".stripMargin

}
