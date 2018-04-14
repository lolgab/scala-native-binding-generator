package bindgen

case class Struct(name: Identifier, components: Seq[Parameter]) extends Definition with HasOps {
  def componentsString: String = components.map(c => s"  ${c.t.withByteString}").mkString(",\n")

  def ptrStr(i: Int): String = s"!ptr._${i + 1}"

  def cStr(t: Type, i: Int): String =
    if (t.HasReferencedTypes) s"(${ptrStr(i)}).cast[$t]" else ptrStr(i)

  def componentsOpsGetterString: Seq[String] = components.zipWithIndex.map {
    case (c, i) =>
      s"  def ${c.nameIdentifier(i + 1)}: ${c.t} = ${cStr(c.t, i)}"
  }

  def componentsOpsSetterString: Seq[String] = components.zipWithIndex.map {
    case (c, i) =>
      s"  def ${c.nameIdentifier(i + 1)}_=(v: ${c.t}): Unit = ${ptrStr(i)} = v${if (c.t.HasReferencedTypes)
        s".cast[${c.t.withByteString}]"
      else ""}"
  }

  override def toString = s"CStruct${components.length}[\n$componentsString\n]"

  def opsString: String =
    s"""implicit class ${name}Ops(val ptr: Ptr[$name]) extends AnyVal {
       |${componentsOpsGetterString.mkString("\n")}
       |
       |${componentsOpsSetterString.mkString("\n")}
       |}""".stripMargin
}
