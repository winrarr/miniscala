package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  val unitType = TupleType(Nil)
  val tab = "   "

  def unparse(n: AstNode, spaces: String = ""): String = n match {
    case VarExp(c) => c
    case IntLit(c) => c.toString
    case BoolLit(c) => c.toString
    case StringLit(c) => c
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp, spaces)
      val rightval = unparse(rightexp, spaces)
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
      val expval = unparse(exp, spaces)
      op match {
        case NegUnOp() => spaces + "- " + expval
        case NotUnOp() => spaces + "!" + expval
      }
    case BlockExp(vals, vars, defs, classes, exps) =>
      val spaces1 = spaces + tab
      var str: String = "{\n"
      for (v <- vals) {
        str += spaces1 + s"val ${v.x} = ${unparse(v.exp, spaces1)};\n"
      }
      for (v <- vars) {
        str += spaces1 + s"var ${v.x} = ${unparse(v.exp, spaces1)};\n"
      }
      for (d <- defs) {
        str += spaces1 + s"def ${d.fun}("
        if (d.params.isEmpty) {
          str += ", "
        }
        for (p <- d.params) {
          str += s", ${p.x}: ${unparse(p.opttype.getOrElse(unitType))}, "
        }
        val resTypeStr = unparse(d.optrestype.getOrElse(unitType))
        if (resTypeStr.equals("")) {
          str = str.substring(0, str.length - 2) + s") = ${unparse(d.body, spaces1)}\n"
        } else {
          str = str.substring(0, str.length - 2) + s"): ${unparse(d.optrestype.getOrElse(unitType))} = ${unparse(d.body, spaces1)}\n"
        }
      }
      for (c <- classes) {
        str += spaces1 + s"class ${c.klass}("
        for (p <- c.params) {
          val paramType = p.opttype.getOrElse(unitType)
          if (paramType != TupleType(List())) {
            str += s"${p.x}: ${unparse(paramType)}, "
          } else {
            str += s"${p.x}, "
          }
        }
        str = str.substring(0, str.length - 2) +
          s") = " + unparse(c.body, spaces1) + ";\n\n"
      }
      for (e <- exps) {
        str += spaces1 + s"${unparse(e, spaces1)} \n"
      }
      str + spaces + "}"
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
    case FunType(paramtypes: List[Type], restype: Type) =>
      s"(${TupleType(paramtypes)}) => ${unparse(restype)}"
    case CallExp(exp, args) => unparse(exp) + argsAsString(args)
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
    case NewObjExp(klass, args) => s"new $klass" + argsAsString(args)
    case LookupExp(objexp, member) => s"${unparse(objexp)}.$member"
    case FunParam(x, opttype) =>
      if (opttype.isEmpty) {
        x
      } else {
        x + ": " + opttype
      }
    case AssignmentExp(x, exp) =>
      s"$x = ${unparse(exp)}"

    case IntType() => "Int"
    case BoolType() => "Boolean"
    case FloatType() => "Float"
    case StringType() => "String"
    case TupleType(types) =>
      if (types.isEmpty) return ""
      var res: String = "("
      for (t <- types) {
        res += unparse(t) + ", "
      }
      res.substring(0, res.length - 2) + ")"
  }

  def argsAsString(args: List[Exp]): String = {
    var res = "("
    for (a <- args) {
      res += unparse(a) + ", "
    }
    if (res.length > 1) {
      res.substring(0, res.length - 2) + ")"
    } else {
      res + ")"
    }
  }
}
