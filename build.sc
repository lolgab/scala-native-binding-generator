import mill._
import mill.scalalib._

trait Common extends ScalaModule {
  def scalaVersion = "2.12.4"

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::fastparse:1.0.0"/*,,
    ivy"com.github.pathikrit::better-files:3.4.0"
    ivy"com.github.scopt::scopt:3.7.0"*/
  )
}

object bindgen extends Common {

  def mainClass = Some("bindgen.Main")

  object test extends Tests {
    def testFrameworks = Seq("utest.runner.Framework")

    def moduleDeps = Seq(bindgen)

    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::utest:0.6.4"
    )
  }
}


