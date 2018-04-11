case class CFunction(returnType: VariableType, name: Identifier, parameters: Seq[Parameter])
    extends Definition {
  override def toString: String = {
    val parametersOutput =
      if (parameters.isEmpty)
        if (returnType.resType == "Unit") "()" else ""
      else {
        val parameterOutput =
          parameters.zipWithIndex.map { case (p, i) => s"${p.nameIdentifier(i)}: ${p.t}" }.mkString(", ")
        s"($parameterOutput)"
      }
    s"def $name$parametersOutput: $returnType = extern"
  }
}
