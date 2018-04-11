case class Struct(name: Identifier, components: Seq[Parameter]) extends Definition with HasOps {
  def componentsString = components.map(c => s"  ${c.t}").mkString(",\n")

  def componentsOpsGetterString = components.zipWithIndex.map {
    case (c, i) => s"  def ${c.nameIdentifier(i + 1)}: ${c.t} = !ptr._${i + 1}"
  }

  def componentsOpsSetterString = components.zipWithIndex.map {
    case (c, i) => s"  def ${c.nameIdentifier(i + 1)}_=(v: ${c.t}): Unit = !ptr._${i + 1} = v"
  }

  override def toString = s"type $name = CStruct${components.length}[\n$componentsString\n]"

  def opsString =
    s"""implicit class ${name}Ops(val ptr: Ptr[$name]) extends AnyVal {
       |${componentsOpsGetterString.mkString("\n")}
       |
       |${componentsOpsSetterString.mkString("\n")}
       |}""".stripMargin
}
