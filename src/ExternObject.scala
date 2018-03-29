case class ExternObject(name: String, definitions: Seq[Definition]) {
  private def hasOpsDefinitions: Seq[HasOps] = definitions.filter(_.isInstanceOf[HasOps]).map(_.asInstanceOf[HasOps])

  def toNative: String =
    s"""import scalanative.native._
       |
       |@extern
       |object $name {
       |${definitions.map(d => s"${d.toNative}").flatMap(_.split('\n')).map(l => s"  $l").mkString("\n")}
       |}
       |
       |object ${name}Ops {
       |  import $name._
       |
       |${hasOpsDefinitions.map(d => s"${d.toNativeOps}").flatMap(_.split('\n')).map(l => s"  $l").mkString("\n")}
       |}""".stripMargin
}
