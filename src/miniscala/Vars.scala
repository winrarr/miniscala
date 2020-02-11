package miniscala

import miniscala.Ast._

/**
  * Computation of free variables.
  */
object Vars {

  def freeVars(e: Exp): Set[Var] = e match {
    case IntLit(_) => Set()
    case VarExp(x) => Set(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case BlockExp(vals, exp) =>
      var fv = freeVars(exp)
      for (d <- vals.reverse)
        fv = fv -- declaredVars(d) ++ freeVars(d)
      fv
  }

  def freeVars(decl: Decl): Set[Var] = decl match {
    case ValDecl(_, exp) => freeVars(exp)
  }

  def declaredVars(decl: Decl): Set[Var] = decl match {
    case ValDecl(x, _) => Set(x)
  }
}
