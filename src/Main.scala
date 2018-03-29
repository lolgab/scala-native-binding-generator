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

    val externObject = ExternObject(objectName, parsed.get.value)

    new PrintWriter(s"${externObject.name}.scala") { write(externObject.toNative); close() }
  }
}
