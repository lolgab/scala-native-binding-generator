import fastparse.WhitespaceApi

object WithSpaces {
  import fastparse.noApi._
  val space = WhitespaceApi.Wrapper {
    NoTrace(WithoutSpaces.space)
  }

  import WithoutSpaces._
  import space._

  val pointer = P("*".rep.!.map(_.count(_ == '*')))

  val variableType =
    P((id.! ~ (id.! ~ &(id)).rep) ~ pointer).map(t => VariableType(t._1 +: t._2, t._3))

  val functionPtrParamenter =
    P(variableType ~ "(" ~ "*" ~ id.! ~ ")" ~ "(" ~ functionParameters ~ ")").map {
      case (returnType, name, parameters) =>
        Parameter(FunctionType(returnType, parameters.map(_.t)), Some(name))
    }

  val variableParameter = P(variableType ~ id.!.?).map(Parameter.tupled)

  val functionParameter = P(functionPtrParamenter | variableParameter)

  val voidParameter = P("void" ~ &(")")).map(_ => Seq.empty[Parameter])

  val functionParameters: Parser[Seq[Parameter]] =
    P(functionParameter.? ~ ("," ~ functionParameter).rep)
      .map {
        case (None, s)    => s
        case (Some(t), s) => t +: s
      }

  val functionDefinition =
    P("extern".? ~ variableType ~ id.! ~ "(" ~ (voidParameter | functionParameters) ~ ")" ~ ";")
      .map(CFunction.tupled)

  val structComponent = P(functionParameter ~ ";")

  val enumComponent = P(id.! ~ ("=" ~ (!"," ~ AnyChar).rep.!).? ~ ",")

  val structBraces = P("{" ~ structComponent.rep ~ "}")

  val enumBraces = P("{" ~ enumComponent.rep ~ "}")

  val structDefinition =
    P("struct" ~ id.?.! ~ structBraces ~ ";").map(Struct.tupled)

  val enumDefinition =
    P("enum" ~ id.?.! ~ enumBraces ~ ";").map(Enum.tupled)

  val typeDefStructDefinition =
    P("typedef" ~ "struct" ~ id.? ~ structBraces ~ id.! ~ ";").map(t => Struct(t._2, t._1))

  val typeDefEnumDefinition =
    P("typedef" ~ "enum" ~ id.? ~ enumBraces ~ id.! ~ ";").map(t => Enum(t._2, t._1))

  val typeDefStructEnumAlias = P("typedef" ~ ("struct" | "enum") ~ id.! ~ id.! ~ ";")
    .map(NameAlias.tupled) /*{ // TODO remove useless typealiases
    case (oldN, newN) =>
      if (oldN == newN) None else Some(TypeAlias(oldN, newN))
  }*/

  val typeDefTypeAlias = P("typedef" ~ variableType ~ id.! ~ ";").map(TypeAlias.tupled)

  val exprContent: Parser[Definition] = P(
    functionDefinition | typeDefStructDefinition | typeDefEnumDefinition | structDefinition | enumDefinition | typeDefStructEnumAlias | typeDefTypeAlias)

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
