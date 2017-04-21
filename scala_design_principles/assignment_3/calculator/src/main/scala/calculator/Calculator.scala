package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (name, expr) <- namedExpressions
    } yield (name, Var(eval(expr(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalHelper(expr: Expr, references: Map[String, Signal[Expr]], explored: Set[Expr]): Double = {
      val newExplored = explored + expr
      if (explored contains expr) Double.NaN
      else
        expr match {
          case Literal(v) => v
          case Ref(name) => evalHelper(getReferenceExpr(name, references), references, newExplored)
          case Plus(a, b) => evalHelper(a, references, newExplored) + evalHelper(b, references, newExplored)
          case Minus(a, b) => evalHelper(a, references, newExplored) - evalHelper(b, references, newExplored)
          case Times(a, b) => evalHelper(a, references, newExplored) * evalHelper(b, references, newExplored)
          case Divide(a, b) => evalHelper(a, references, newExplored) / evalHelper(b, references, newExplored)
        }
    }

    evalHelper(expr, references, Set[Expr]())

  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
