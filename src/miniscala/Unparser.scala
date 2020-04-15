package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: AstNode): String = n match {
    case IntLit(c) => c.toString
    case BoolLit(c) => c.toString
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
        case AndBinOp() => leftval + " && " + rightval
        case OrBinOp() => leftval + " || " + rightval
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => "- " + expval
        case NotUnOp() => "!" + expval
      }
    case BlockExp(vals, vars, defs, exps) =>
      var str: String = "{ "
      for (v <- vals) {
        str += s"val ${v.x} = ${unparse(v.exp)};\n"
      }
      for (v <- vars) {
        str += s"var ${v.x} = ${unparse(v.exp)};\n"
      }
      for (d <- defs) {
        str += s"def ${d.fun}("
        for (p <- d.params) {
          str += s"${p.x}: ${p.opttype.getOrElse("")}, "
        }
        str = str.substring(0, str.length - 2) +
          s"): ${d.optrestype.getOrElse("")} = { ${d.body} };\n"
      }
      for (e <- exps) {
        str += s"${unparse(e)};\n"
      }
      str + " }"
    case VarExp(x) => x.toString
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
    case CallExp(exp, args) =>
      var res: String = unparse(exp) + "("
      for (a <- args) {
        res = res + unparse(a)
      }
      res + ")"
    case LambdaExp(params, body) =>
      var res: String = ""
      for (p <- params) {
        if (p.opttype.isEmpty) {
          res = res + s"(${p.x}) => "
        } else {
          res = res + s"(${p.x}: ${p.opttype}) => "
        }
      }
      res + unparse(body)
    case FunParam(x, opttype) =>
      if (opttype.isEmpty) {
        x
      } else {
        x + ": " + opttype
      }
  }
}
