package preprocessor

import fastparse.WhitespaceApi
import fastparse.noApi._

object PreProcessor {
  val space = {
    import fastparse.all._
    (" " | "\t").rep
  }
  val WhiteSpace = WhitespaceApi.Wrapper(NoTrace(space))

  import WhiteSpace._

  val anything = (AnyChar ~ !space).rep

  val define = P(
    "#define" ~ anything.! ~ anything.!
  )

  val path = P(
    (AnyChar ~ !"/").rep.!
  )

  val includeLocal = P(
    "#include" ~ "\"" ~ path ~ "\""
  )

  val includeSystem = P(
    "#include" ~ "<" ~ path ~ ">"
  )

  val prep = P(
    (define | includeLocal | includeSystem) ~ "\n"
  )

  val expr = P((prep | AnyChar).rep)
}
