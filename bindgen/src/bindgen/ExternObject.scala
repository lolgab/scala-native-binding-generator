package bindgen

case class ExternObject(name: String, definitions: Seq[Definition]) {
  private lazy val hasOpsDefinitions: Seq[HasOps] =
//    definitions.filter(_.isInstanceOf[HasOps]).map(_.asInstanceOf[HasOps])
    definitions
      .filter {
        case ops: HasOps                  => true
        case TypeAlias(name, ops: HasOps) => true
        case _                            => false
      }
      .map {
        case TypeAlias(name, ops: HasOps) => ops
        case d                            => d
      }
      .asInstanceOf[Seq[HasOps]]

  private def obsObject: String =
    s"""
       |object ${name}Ops {
       |import $name._
       |
       |${hasOpsDefinitions
         .map(d => s"${d.opsString}")
         .flatMap(_.split('\n'))
         .map(l => s"  $l")
         .mkString("\n")}
       |}""".stripMargin

  override def toString: String =
    s"""import scalanative.native._
       |
       |@extern
       |object $name {
       |${definitions.map(_.toString).flatMap(_.split('\n')).map(l => s"  $l").mkString("\n")}
       |}
       |${if (hasOpsDefinitions.nonEmpty) obsObject else ""}""".stripMargin
}
