package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: AstNode): String = {
    n match {
      case IntLit(c) => c.toString
      case BinOpExp(leftexp, op, rightexp) =>
        op match {
          case PlusBinOp() =>
            unparse(leftexp) + " + " + unparse(rightexp)
          case MinusBinOp() =>
            unparse(leftexp) + " - " + unparse(rightexp)
          case MultBinOp() =>
            unparse(leftexp) + " * " + unparse(rightexp)
          case DivBinOp() =>
            unparse(leftexp) + " / " + unparse(rightexp)
          case ModuloBinOp() =>
            unparse(leftexp) + " % " + unparse(rightexp)
          case MaxBinOp() =>
            unparse(leftexp) + " max " + unparse(rightexp)
        }
      case UnOpExp(op, exp) =>
        val expval = unparse(exp)
        op match {
          case NegUnOp() => "- " + expval
        }
    }
  }
}
