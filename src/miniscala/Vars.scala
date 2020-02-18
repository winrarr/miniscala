package miniscala

import miniscala.Ast._

/**
  * Computation of free variables.
  */
object Vars {

  def freeVars(e: Exp): Set[Var] = e match {
    case _: Literal => Set()
    case VarExp(x) => Set(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => freeVars(condexp) ++ freeVars(thenexp) ++ freeVars(elseexp)
    case BlockExp(vals, exp) =>
      var fv = freeVars(exp)
      for (d <- vals.reverse)
        fv = fv -- declaredVars(d) ++ freeVars(d)
      fv
    case TupleExp(exps) =>
      var fv = Set[Var]()
      for (exp <- exps)
        fv = fv ++ freeVars(exp)
      fv
    case MatchExp(exp, cases) =>
      var fv = freeVars(exp)
      for (c <- cases)
        fv = fv ++ (freeVars(c.exp) -- c.pattern)
      fv
  }

  def freeVars(decl: Decl): Set[Var] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
  }

  def declaredVars(decl: Decl): Set[Var] = decl match {
    case ValDecl(x, _, _) => Set(x)
  }
}
