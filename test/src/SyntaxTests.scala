import java.io.PrintWriter

import fastparse.core.Parser
import utest._

object SyntaxTests extends TestSuite {
  import WithSpaces._

  def toNative(parser: Parser[Definition, Char, String], s: String): String =
    parser.parse(s).get.value.toNative

  def toNativeOps(parser: Parser[HasOps, Char, String], s: String): String =
    parser.parse(s).get.value.toNativeOps

  def tests = Tests {
    "pointer type" - {
      val s        = "int *"
      val res      = toNative(variableType, s)
      val shouldBe = "Ptr[CInt]"
      assert(shouldBe == res)
    }

    "normal type" - {
      val s        = "long"
      val res      = toNative(variableType, s)
      val shouldBe = "CLong"
      assert(shouldBe == res)
    }

    "double word type" - {
      val s        = "long long"
      val res      = toNative(variableType, s)
      val shouldBe = "CLong"
      assert(shouldBe == res)
    }

    "function definition" - {
      val s   = "void * foo(int a, char** b, void (*f)(int, char*, long), void* data);"
      val res = toNative(functionDefinition, s)
      val shouldBe =
        "def foo(a: CInt, b: Ptr[CString], f: CFunctionPtr3[CInt, CString, CLong, Unit], data: Ptr[Byte]): Ptr[Byte] = extern"

      assert(res == shouldBe)
    }

    "function void parameter" - {
      val s        = "extern void f(void);"
      val res      = toNative(functionDefinition, s)
      val shouldBe = "def f(): Unit = extern"
      assert(res == shouldBe)
    }

    "function definition" - {
      val s = "extern void uiQueueMain(void (*f)(void *data), void *data);"
      val res = toNative(functionDefinition, s)
      val shouldBe = "def uiQueueMain(f: CFunctionPtr1[Ptr[Byte], Unit], data: Ptr[Byte]): Unit = extern"
      assert(res == shouldBe)
    }

    "function" - {
      val s   = "void uiBoxAppend (uiBox *b, uiControl *child, int stretchy);"
      val res = toNative(functionDefinition, s)
      val shouldBe =
        "def uiBoxAppend(b: Ptr[uiBox], child: Ptr[uiControl], stretchy: CInt): Unit = extern"
      assert(res == shouldBe)
    }

    "struct definition" - {
      val struct = """struct Foo {
                     |  int a;
                     |  long b;
                     |  my_type c;
                     |  char * s;
                     |};""".stripMargin

      val shouldBeNative =
        """type Foo = CStruct4[
          |  CInt,
          |  CLong,
          |  my_type,
          |  CString
          |]""".stripMargin

      val shouldBeNativeOps =
        """implicit class FooOps(ptr: Ptr[Foo]) extends AnyVal {
          |  def a: CInt = !ptr._1
          |  def b: CLong = !ptr._2
          |  def c: my_type = !ptr._3
          |  def s: CString = !ptr._4
          |
          |  def a_=(v: CInt): Unit = !ptr._1 = v
          |  def b_=(v: CLong): Unit = !ptr._2 = v
          |  def c_=(v: my_type): Unit = !ptr._3 = v
          |  def s_=(v: CString): Unit = !ptr._4 = v
          |}""".stripMargin

      "type definition in the extern object" - {
        val res = toNative(structDefinition, struct)
        assert(res == shouldBeNative)
      }

      "implicit class in the ops object" - {
        val res = toNativeOps(structDefinition, struct)
        assert(res == shouldBeNativeOps)
      }

      "extern object" - {
        val res               = ExternObject("MyObj", expr.parse(struct).get.value).toNative
        def indent(s: String) = s.split('\n').map(s => s"  $s").mkString("\n")

        val shouldBeExternObject =
          s"""import scalanative.native._
          |
          |@extern
          |object MyObj {
          |${indent(shouldBeNative)}
          |}
          |
          |object MyObjOps {
          |  import MyObj._
          |
          |${indent(shouldBeNativeOps)}
          |}""".stripMargin

        assert(res == shouldBeExternObject)
      }
    }
  }
}
