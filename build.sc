import mill._
import mill.scalalib._
import ammonite.ops._

trait Common extends ScalaModule {
  def scalaVersion = "2.12.4"

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::fastparse:1.0.0"/*,
    ivy"com.github.scopt::scopt:3.7.0"*/
  )
}

object build extends Common {
  def millSourcePath = pwd

  def mainClass = Some("Main")

  object test extends Tests {
    def testFrameworks = Seq("utest.runner.Framework")

    def moduleDeps = Seq(build)

    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::utest:0.6.4"
    )
  }
}


