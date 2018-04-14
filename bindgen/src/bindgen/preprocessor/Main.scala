package bindgen.preprocessor

import java.io.PrintWriter

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    import PreProcessor._

    val lines = Source
      .fromFile(args(0))
      .getLines()

    val s = lines.mkString("\n")

    val parsed = expr.parse(s)

    //    parsed match {
    //      case f: Failure[_, _] =>
    //        new PrintWriter("err.txt") {write(f.toString); close}
    //        System.err.println(f)
    //    }

    val objectName = args(0).split('.').head

    val definitions = transformed(parsed.get.value).mkString("\n")

    new PrintWriter(s"$objectName.NEW.h") { write(definitions); close() }
  }

  def transformed(definitions: Seq[PreprocessorDefinition]): Seq[PreprocessorDefinition] = {
    definitions.filter{
      case Define(_, s) if s == "" =>  false
      case l: Line => false
      case _ => true
    }
  }
}