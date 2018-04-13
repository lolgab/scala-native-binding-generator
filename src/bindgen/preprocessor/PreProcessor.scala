package bindgen.preprocessor

import bindgen.Identifier
import fastparse.WhitespaceApi
import fastparse.noApi._

object PreProcessor {
  val space = {
    import fastparse.all._
    (" " | "\t").rep
  }
  val WhiteSpace = WhitespaceApi.Wrapper(NoTrace(space))


  import WhiteSpace._

  val anything = {
    import fastparse.all._
    (!("\n") ~ AnyChar).rep
  }

  val define = P(
    "#define" ~ anything.! ~ anything.?.!
  ).map(t => Define(Identifier(t._1), t._2))

  val includeLocal = P(
    "#include" ~ "\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\""
  )

  val includeSystem = P(
    "#include" ~ "<" ~ (!">" ~ AnyChar).rep.! ~ ">"
  )

  val line = P (
    (!"\n" ~ AnyChar).rep.!.map(Line.apply)
  )

  val prep = P(
    (define | line /*| includeLocal | includeSystem*/) ~ "\n"
  )

  val expr = P(space ~ (prep).rep ~ space)
}
