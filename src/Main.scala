import java.io.PrintWriter

import fastparse.core.Parsed.{Failure, Success}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    import WithSpaces._

    val lines = Source
      .fromFile(args(0))
      .getLines()

    val s = lines
      .mkString("\n")

    val parsed = expr.parse(s)

//    parsed match {
//      case f: Failure[_, _] =>
//        new PrintWriter("err.txt") {write(f.toString); close}
//        System.err.println(f)
//    }

    val objectName = args(0).split('.').head

    val definitions = parsed.get.value

    val externObject = ExternObject(objectName, DefinitionsUtils.withoutUselessNameAliases(definitions))

    new PrintWriter(s"${externObject.name}.scala") { write(externObject.toString); close() }
  }
}
