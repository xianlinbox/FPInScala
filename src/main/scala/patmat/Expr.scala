package patmat

trait Expr

case class Number(n: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Prod(left: Expr, right: Expr) extends Expr

case class Var(x: String) extends Expr


object Exprs {
  def show(e: Expr, prodPre:Boolean): String = e match {
    case Number(n) => n.toString()
    case Sum(left, right) =>
      if (prodPre) "("+ show(left, false) + "+" + show(right,false) + ")"
      else show(left, false) + "+" + show(right,false)
    case Prod(left, right) => show(left, true) + "*" + show(right, true)
    case Var(n) => n
  }
}

object Main {
  def main(args: Array[String]) {
    //2 * x +y
    println(Exprs.show(Sum(Prod(Number(2), Var("x")), Var("y")), false))

    //(2+x)*y
    println(Exprs.show(Prod(Sum(Number(2), Var("x")), Var("y")),false))

    //(2+x)*y*z
    println(Exprs.show(Prod(Prod(Sum(Number(2), Var("x")), Var("y")), Var("z")),false))

    //(2+x)*(4+y)
    println(Exprs.show(Prod(Sum(Number(2), Var("x")), Sum(Number(4),Var("y"))),false))
  }
}

