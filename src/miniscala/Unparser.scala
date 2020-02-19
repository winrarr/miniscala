package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: AstNode): String = n match {
    case IntLit(c) => c.toString
    case BoolLit(c) => c.toString
    case FloatLit(c) => c.toString
    case StringLit(c) => c
    case VarExp(x) => x
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp)
      val rightval = unparse(rightexp)
      op match {
        case PlusBinOp() => leftval + " + " + rightval
        case MinusBinOp() => leftval + " - " + rightval
        case MultBinOp() => leftval + " * " + rightval
        case DivBinOp() => leftval + " / " + rightval
        case ModuloBinOp() => leftval + " % " + rightval
        case MaxBinOp() => leftval + " max " + rightval
        case EqualBinOp() => leftval + " == " + rightval
        case LessThanBinOp() => leftval + " < " + rightval
        case LessThanOrEqualBinOp() => leftval + " <= " + rightval
        case AndBinOp() => leftval + " & " + rightval
        case OrBinOp() => leftval + " | " + rightval
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => "- " + expval
        case NotUnOp() => "!" + expval
      }
    case BlockExp(vals, exp) =>
      var valsString: String = "{ "
      for (v <- vals) {
        valsString += s"val ${v.x} = ${unparse(v.exp)}; "
      }
      valsString + unparse(exp) + " }"
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      s"if (${unparse(condexp)}) ${unparse(thenexp)} else ${unparse(elseexp)}"
    case TupleExp(exps) =>
      var res = "("
      for (e <- exps) {
        res += unparse(e) + ", "
      }
      res.substring(0, res.length - 2) + ")"
    case MatchExp(exp, cases) =>
      var res = unparse(exp) + " match {"
      for (c <- cases) {
        res += " case ("
        for (e <- c.pattern) {
          res += e + ", "
        }
        res = res.substring(0, res.length - 2) + ") => " + unparse(c.exp) + ";"
      }
      res + " }"
    case IntType() => "Int"
    case BoolType() => "Bool"
    case FloatType() => "Float"
    case StringType() => "String"
    case TupleType(types) => types.length + "-Tuple"
  }
}
