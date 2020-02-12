package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: Exp): String = {
    n match {
      case IntLit(c) => c.toString
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
        }
      case UnOpExp(op, exp) =>
        val expval = unparse(exp)
        op match {
          case NegUnOp() => "- " + expval
        }
      case BlockExp(vals, exp) =>
        var valsString: String = "{ "
        for (v <- vals) {
          valsString += s"val ${v.x} = ${unparse(v.exp)}; "
        }
        valsString + unparse(exp) + " }"
      case VarExp(x) => x.toString
    }
  }
}
