import fastparse.WhitespaceApi

object WithSpaces {
  import fastparse.noApi._
  val space = WhitespaceApi.Wrapper {
    NoTrace(WithoutSpaces.space)
  }

  import WithoutSpaces._
  import space._

  val identifier    = id.!.map(Identifier.apply)
  val optIdentifier = id.?.!.map(Identifier.apply)

  val pointer = P("*".rep.!.map(_.count(_ == '*')))

  val variableType =
    P((id.! ~ (id.! ~ &(id)).rep) ~ pointer).map(t => VariableType(t._1 +: t._2, t._3))

  val functionPtr = P(variableType ~ "(" ~ "*" ~ identifier ~ ")" ~ "(" ~ functionParameters ~ ")")

  val functionPtrParamenter = functionPtr.map {
    case (returnType, name, parameters) =>
      Parameter(FunctionType(returnType, parameters.map(_.t)), Some(name))
  }

  val array = P("[" ~ digit.rep.! ~ "]").map(_.toInt)

  val variableParameter = P(variableType ~ identifier.? ~ array.?)
    .map(
      t =>
        if (t._3.isDefined)
          (t._1.copy(pointerCount = t._1.pointerCount + 1), t._2)
        else (t._1, t._2))
    .map(Parameter.tupled)

  val functionParameter = P(functionPtrParamenter | variableParameter)

  val voidParameter = P("void" ~ &(")")).map(_ => Seq.empty[Parameter])

  val functionParameters: Parser[Seq[Parameter]] =
    P(functionParameter.? ~ ("," ~ functionParameter).rep)
      .map {
        case (None, s)    => s
        case (Some(t), s) => t +: s
      }

  val functionDefinition: Parser[Seq[Definition]] =
    P("extern".? ~ variableType ~ identifier ~ "(" ~ (voidParameter | functionParameters) ~ ")" ~ ";")
      .map(CFunction.tupled)
      .map(Seq(_))

  val structComponent = P(functionParameter ~ ";")

  val enumComponent = P(identifier ~ ("=" ~ (!"," ~ AnyChar).rep.!).? ~ ",")

  val structBraces = P("{" ~ structComponent.rep ~ "}")

  val enumBraces = P("{" ~ enumComponent.rep ~ "}")

  val structDefinition: Parser[Seq[Struct]] =
    P("struct" ~ identifier ~ structBraces ~ ";").map(Struct.tupled).map(Seq(_))

  val enumDefinition: Parser[Seq[Definition]] =
    P("enum" ~ optIdentifier ~ enumBraces ~ ";").map(Enum.tupled).map(Seq(_))

  val typeDefStructDefinition: Parser[Seq[Definition]] =
    P("typedef" ~ "struct" ~ identifier.? ~ structBraces ~ identifier ~ ";").map(t =>
      t._1 match {
        case Some(name) => Seq(Struct(name, t._2), NameAlias(name, t._3))
        case None       => Seq(Struct(t._3, t._2))
    })

  val typeDefFunctionPtrDefinition: Parser[Seq[Definition]] =
    P("typedef" ~ functionPtr ~ ";").map {
      case (retType, name, params) =>
        Seq(FunctionPtrDefinition(FunctionType(retType, params.map(_.t)), name))
    }

  val typeDefEnumDefinition: Parser[Seq[Definition]] =
    P("typedef" ~ "enum" ~ optIdentifier ~ enumBraces ~ identifier ~ ";").map(t =>
      t._1 match {
        case Identifier("") => Seq(Enum(t._3, t._2))
        case name           => Seq(Enum(t._1, t._2), NameAlias(name, t._3))
    })

  val typeDefStructEnumAlias: Parser[Seq[Definition]] =
    P("typedef" ~ ("struct" | "enum") ~ identifier ~ identifier ~ ";")
      .map(NameAlias.tupled)
      .map(Seq(_))

  val typeDefTypeAlias: Parser[Seq[Definition]] =
    P("typedef" ~ variableType ~ identifier ~ ";").map(TypeAlias.tupled).map(Seq(_))

  val exprContent: Parser[Seq[Definition]] = P(
    functionDefinition |
      typeDefStructDefinition |
      typeDefEnumDefinition |
      structDefinition |
      enumDefinition |
      typeDefStructEnumAlias |
      typeDefTypeAlias |
      typeDefFunctionPtrDefinition)

  val expr = P(WithoutSpaces.space ~ exprContent.rep ~ WithoutSpaces.space ~ End)
}

object WithoutSpaces {
  import fastparse.all._
  val lowerCase = P(CharIn('a' to 'z'))
  val upperCase = P(CharIn('A' to 'Z'))
  val nonDigit  = P(lowerCase | upperCase | "_")
  val digit     = P(CharIn('0' to '9'))
  val id        = P(nonDigit ~ (nonDigit | digit).rep)

  val multiLineComment  = P("/*" ~ (!"*/" ~ AnyChar).rep ~ "*/")
  val singleLineComment = P("//" ~ (!"\n" ~ AnyChar).rep ~ ("\n" | End))
  val comment           = P(singleLineComment | multiLineComment)
  val keyword           = P("const")
  val space             = P(" " | "\t" | "\n" | comment | keyword).rep
}
