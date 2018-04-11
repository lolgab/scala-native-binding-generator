case class ExternObject(name: String, definitions: Seq[Definition]) {
  private def hasOpsDefinitions: Seq[HasOps] = definitions.filter(_.isInstanceOf[HasOps]).map(_.asInstanceOf[HasOps])

  override def toString: String =
    s"""import scalanative.native._
       |
       |@extern
       |object $name {
       |${definitions.map(_.toString).flatMap(_.split('\n')).map(l => s"  $l").mkString("\n")}
       |}
       |
       |object ${name}Ops {
       |  import $name._
       |
       |${hasOpsDefinitions.map(d => s"${d.opsString}").flatMap(_.split('\n')).map(l => s"  $l").mkString("\n")}
       |}""".stripMargin
}
