sealed trait Type extends HasNative {
  def cyclicReference: Boolean
}

case class VariableType(types: Seq[String], pointerCount: Int, cyclicReference: Boolean = false)
    extends Type {
  val resType = {
    implicit val seq: Seq[String]                  = types.sorted
    def cmp(s: String*)(implicit seq: Seq[String]) = s.sorted == seq
    if (cmp("void") && pointerCount > 0) "Byte"
    else if (cmp("void")) "Unit"
    else if (cmp("bool")) "CBool"
    else if (cmp("char") && pointerCount > 0) "CString"
    else if (cmp("char")) "CChar"
    else if (cmp("char", "signed")) "CSignedChar"
    else if (cmp("char", "unsigned")) "CUnsignedChar"
    else if (cmp("short")) "CShort"
    else if (cmp("unsigned", "short")) "CUnsignedShort"
    else if (cmp("int")) "CInt"
    else if (cmp("long", "int")) "CLongInt"
    else if (cmp("unsigned", "int") || cmp("uint32_t")) "CUnsignedInt"
    else if (cmp("long", "int", "unsigned")) "CUnsignedLongInt"
    else if (cmp("long")) "CLong"
    else if (cmp("long", "unsigned") || cmp("uint64_t")) "CUnsignedLong"
    else if (cmp("long", "long")) "CLongLong"
    else if (cmp("unsigned", "long", "long")) "CUnsignedLongLong"
    else if (cmp("size_t")) "CSize"
    else if (cmp("ptrdiff_t")) "CPtrDiff"
    else if (cmp("wchar_t")) "CWideChar"
    else if (cmp("char16_t")) "CChar16"
    else if (cmp("char32_t")) "CChar32"
    else if (cmp("float")) "CFloat"
    else if (cmp("double")) "CDouble"
    else seq.mkString(" ")
  }

  val ptrCount = if (resType == "CString") pointerCount - 1 else pointerCount
  (resType, ptrCount)

  override def toString: String = {
    def loop(s: String, i: Int): String =
      if (i == 0) s
      else loop(s"Ptr[$s]", i - 1)
    loop(resType, ptrCount)
  }
}

case class FunctionType(returnType: Type, parameters: Seq[Type]) extends Type {
  override def cyclicReference: Boolean =
    parameters.foldLeft(returnType.cyclicReference)(_ || _.cyclicReference)

  def parametersString = parameters.map(_.toString).mkString(", ")

  override def toString: String =
    s"CFunctionPtr${parameters.length}[$parametersString, $returnType]"
}
