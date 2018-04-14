package bindgen

import fastparse.WhitespaceApi
import fastparse.noApi._

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
    P(("struct" | "enum").? ~ (id.! ~ (id.! ~ &(id)).rep) ~ pointer).map(t =>
      VariableType(t._1 +: t._2, t._3))

  val functionPtr =
    P(variableType ~ "(" ~ "*" ~ identifier ~ ")" ~ "(" ~ functionParameters ~ ")").map {
      case (returnType, name, parameters) =>
        (FunctionType(returnType, parameters.map(_.t)), Some(name))
    }

  val functionPtrParameter = functionPtr.map(Parameter.tupled)

  val array = P("[" ~ digit.rep.! ~ "]").map(_.toInt)

  val variableParameter = P(variableType /*~ digit.rep */~ identifier.? ~ array.?)
    .map(
      t =>
        if (t._3.isDefined)
          (t._1.copy(pointerCount = t._1.pointerCount + 1), t._2)
        else (t._1, t._2))
    .map(Parameter.tupled)

  val functionParameter = P(functionPtrParameter | variableParameter)

  val voidParameter = P("void" ~ &(")")).map(_ => Seq.empty[Parameter])

  val functionParameters: Parser[Seq[Parameter]] =
    P(functionParameter.? ~ ("," ~ functionParameter).rep)
      .map {
        case (None, s)    => s
        case (Some(t), s) => t +: s
      }

  val functionDefinition: Parser[Definition] =
    P(variableType ~ identifier ~ "(" ~ (voidParameter | functionParameters) ~ ")")
      .map(CFunction.tupled)

  val structComponent = P(functionParameter ~ ";")

  val enumComponent = P(identifier ~ ("=" ~ (!"," ~ AnyChar).rep.!).? ~ ",")

  val structBraces = P("{" ~ structComponent.rep ~ "}")

  val enumBraces = P("{" ~ enumComponent.rep ~ "}")

  val emptyStructDefinition =
    P("struct" ~ identifier).map(TypeAlias(_, ExternDefinition))

  val fullStructDefinition: Parser[Struct] =
    P("struct" ~ optIdentifier ~ structBraces).map(Struct.tupled)

  val structDefinition = P(fullStructDefinition | emptyStructDefinition)

  val enumDefinition: Parser[Definition] =
    P("enum" ~ optIdentifier ~ enumBraces).map(Enum.tupled)

  val typedefableDefinition = P(
    variableType |
      structDefinition |
      enumDefinition
  )

  val typeDef = P("typedef" ~ typedefableDefinition ~ identifier).map(t => TypeAlias(t._2, t._1))

  val typeDefFunctionPtrDefinition: Parser[Definition] =
    P("typedef" ~ functionPtr).map {
      case (fType, optIde) =>
        TypeAlias(optIde.get, fType) // TODO Unsafe!
    }

  val typeDefStructEnumAlias: Parser[Definition] = // TODO DRY it.
    P("typedef" ~ ("struct" | "enum") ~ identifier ~ identifier)
      .map(t => TypeAlias(t._2, t._1))

  val exprContent: Parser[Definition] = P(
    functionDefinition |
      structDefinition |
      enumDefinition |
      typeDefFunctionPtrDefinition |
      typeDefStructEnumAlias |
      typeDef
  )

  val expr = P(WithoutSpaces.space ~ (exprContent ~ ";").rep ~ WithoutSpaces.space ~ End)
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
  val keyword           = P("const" | "extern")
  val space             = P(" " | "\t" | "\n" | comment | keyword).rep
}
