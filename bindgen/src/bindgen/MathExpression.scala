package bindgen

import fastparse.noApi._
//import WithoutSpaces._
import WithSpaces.space._

object MathExpression {
  def eval(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (op1, (op, op2)) =>
        op match {
          case "*"  => op1 * op2
          case "/"  => op1 / op2
          case "+"  => op1 + op2
          case "-"  => op1 - op2
          case ">>" => op1 >> op2
          case "<<" => op1 << op2
          case "|"  => op1 | op2
          case "&"  => op1 & op2
          case "^"  => op1 ^ op2
        }
    }
  }

  val number: P[Int]     = P(CharIn('0' to '9').rep(1).!.map(_.toInt))
  val factor: P[Int]     = P(number | parens)
  val divMul: P[Int]     = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)
  val addSub: P[Int]     = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
  val shifts: P[Int]     = P(addSub ~ ((">>" | "<<").! ~/ addSub).rep).map(eval)
  val bitwiseAnd: P[Int] = P(shifts ~ ("&".! ~/ shifts).rep).map(eval)
  val bitwiseXor: P[Int] = P(bitwiseAnd ~ ("^".! ~/ bitwiseAnd).rep).map(eval)
  val bitwiseOr: P[Int]  = P(bitwiseXor ~ ("|".! ~/ bitwiseXor).rep).map(eval)
  val parens: P[Int]     = P("(" ~/ addSub ~ ")")
  val expr: P[Int]       = P(bitwiseOr)
}
