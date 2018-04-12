object DefinitionsUtils {
  def withoutUselessNameAliases(definitions: Seq[Definition]): Seq[Definition] = {
    val p1 = definitions
      .map {
        case alias: NameAlias =>
          if (definitions
                .filter(_ != alias)
                .forall(d => d.name != alias.name)) ExternTypeAlias(alias.newName)
          else alias
        case d => d
      }
      .filter {
        case NameAlias(a, b) if a == b => false
        case _                         => true
      }
    withoutCyclicDefinitions(p1)
  }

  def withoutCyclicDefinitions(definitions: Seq[Definition]): Seq[Definition] = {
    def withCyclicReferenceType(t: Type, reference: Identifier): Type = t match {
      case v: VariableType =>
        v.copy(cyclicReferencedTypes = v.cyclicReferencedTypes :+ reference)
      case FunctionType(returnType, parameters) =>
        FunctionType(withCyclicReferenceType(returnType, reference),
                     parameters.map(withCyclicReferenceType(_, reference)))
    }

    definitions.map {
      case s: Struct =>
        val res = s.copy(
          components = s.components.map(c => c.copy(t = withCyclicReferenceType(c.t, s.name))))
        res
      case d => d
    }
  }
}
