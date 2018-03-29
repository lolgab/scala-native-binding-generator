case class Struct(name: String, components: Seq[Parameter]) extends Definition with HasOps {
  def componentsString = components.map(c => s"  ${c.t.toNative}").mkString(",\n")

  def componentsOpsGetterString = components.zipWithIndex.map {
    case (c, i) => s"  def ${c.nameString(i + 1)}: ${c.t.toNative} = !ptr._${i + 1}"
  }

  def componentsOpsSetterString = components.zipWithIndex.map {
    case (c, i) => s"  def ${c.nameString(i + 1)}_=(v: ${c.t.toNative}): Unit = !ptr._${i + 1} = v"
  }

  def toNative = s"type $name = CStruct${components.length}[\n$componentsString\n]"

  def toNativeOps =
    s"""implicit class ${name}Ops(ptr: Ptr[$name]) extends AnyVal {
       |${componentsOpsGetterString.mkString("\n")}
       |
       |${componentsOpsSetterString.mkString("\n")}
       |}""".stripMargin
}
