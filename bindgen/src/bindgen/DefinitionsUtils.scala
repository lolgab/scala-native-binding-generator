package bindgen

object DefinitionsUtils {
  def transformed(definitions: Seq[Definition]): Seq[Definition] = {
    val r1 = withoutUselessNameAliases(definitions)
    val r2 = withoutCyclicDefinitions(r1)
    val r3 = deepPtrVoidSubstitution(r2)
    r3
  }

  def withoutUselessNameAliases(definitions: Seq[Definition]): Seq[Definition] = {
    definitions
      .map {
        case alias: TypeAlias
            if !ScalaNativeDefaultTypes.seq.contains(alias.content.toString) &&
              definitions
                .filter(_ != alias)
                .forall(d =>
                  alias.content.isInstanceOf[Definition] && d.name != alias.content
                    .asInstanceOf[Definition]
                    .name) =>
          alias.copy(content = ExternDefinition)
        case s: Struct => TypeAlias(s.name, s)
        case TypeAlias(name, Struct(sName, params)) =>
          TypeAlias(name, Struct(name, params))
        case d => d
      }
      .filter {
        case TypeAlias(a, b) if a == b => false
        case _                         => true
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

  def deepPtrVoidSubstitution(definitions: Seq[Definition]): Seq[Definition] = {
    def voidTypesLoop(aliases: Seq[TypeAlias], acc: Seq[String]): Seq[String] = {
      val (voids, noVoids) = aliases.partition {
        case TypeAlias(_, vt: VariableType)
            if vt.types.length == 1 && acc.contains(vt.types.head) =>
          true
        case _ => false
      }
      if (voids.isEmpty) acc
      else {
        val newAcc = acc ++: voids.map(_.name.name)
        newAcc ++: voidTypesLoop(noVoids, newAcc)
      }
    }

    val voidTypes = voidTypesLoop(definitions
                                    .filter {
                                      case TypeAlias(_, vt: VariableType) => true
                                      case _                              => false
                                    }
                                    .asInstanceOf[Seq[TypeAlias]],
                                  Seq("void"))

    def toPtrByte(tpe: Type): Type = {
      tpe match {
        case VariableType(Seq(t), n, p) if voidTypes.contains(t) && n > 0 =>
          VariableType(Seq("void"), n, p)
        case FunctionType(returnType, parameters) =>
          FunctionType(toPtrByte(returnType), parameters.map(toPtrByte))
        case t => t
      }
    }

    def toPtrByteParameters(parameters: Seq[Parameter]): Seq[Parameter] =
      parameters.map(p => p.copy(t = toPtrByte(p.t)))

    definitions.map {
      case CFunction(returnType, name, parameters) =>
        CFunction(toPtrByte(returnType).asInstanceOf[VariableType],
                  name,
                  toPtrByteParameters(parameters))
      case Struct(name, parameters) => Struct(name, toPtrByteParameters(parameters))
      case TypeAlias(n1, Struct(n2, parameters)) =>
        TypeAlias(n1, Struct(n2, toPtrByteParameters(parameters)))
      case TypeAlias(name, FunctionType(returnType, parameters)) =>
        TypeAlias(name, FunctionType(toPtrByte(returnType), parameters.map(toPtrByte)))
      //TODO do for all the definition types
      case d => d
    }
  }
}
