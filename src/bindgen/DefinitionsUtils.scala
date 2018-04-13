package bindgen

object DefinitionsUtils {
  def transformed(definitions: Seq[Definition]): Seq[Definition] = {
    val r1 = withoutUselessNameAliases(definitions)
    val r2 = withoutCyclicDefinitions(r1)
    r2
  }

  def withoutUselessNameAliases(definitions: Seq[Definition]): Seq[Definition] = {
    definitions
      .map {
        case alias: TypeAlias =>
          if (definitions
                .filter(_ != alias)
                .forall(d => d.name != alias.name)) alias.copy(content = ExternDefinition)
          else alias
        case s: Struct => TypeAlias(s.name, s)
        case d         => d
      }
      .filter {
        case TypeAlias(a, Identifier(b)) if a == b => false
        case _                                     => true
      }
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
