case class CFunction(returnType: VariableType, name: String, parameters: Seq[Parameter])
    extends Definition {
  def toNative: String = {
    val parametersOutput =
      if (parameters.isEmpty)
        if (returnType.resType == "Unit") "()" else ""
      else {
        val parameterOutput =
          parameters.zipWithIndex.map { case (p, i) => s"${p.nameString(i)}: ${p.t.toNative}" }.mkString(", ")
        s"($parameterOutput)"
      }
    s"def $name$parametersOutput: ${returnType.toNative} = extern"
  }
}
