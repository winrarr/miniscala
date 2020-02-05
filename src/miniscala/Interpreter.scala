package miniscala

import miniscala.Ast._

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  def eval(e: Exp): Int = e match {
    case IntLit(c) => c
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp)
      val rightval = eval(rightexp)
      op match {
        case PlusBinOp() =>
          trace(s"Adding $rightval to $leftval")
          leftval + rightval
        case MinusBinOp() =>
          trace(s"Subtracting $rightval from $leftval")
          leftval - rightval
        case MultBinOp() =>
          trace(s"Multiplying $leftval and $rightval")
          leftval * rightval
        case DivBinOp() =>
          trace(s"Dividing $leftval with $rightval")
          if (rightval == 0)
            throw new InterpreterError(s"Division by zero", op)
          leftval / rightval
        case ModuloBinOp() =>
          trace(s"Processing $leftval mod $rightval")
          leftval % rightval
        case MaxBinOp() =>
          trace(s"Processing max value of $leftval to $rightval")
          if (leftval > rightval) leftval else rightval
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp)
      op match {
        case NegUnOp() =>
          trace(s"Negating $exp")
          -expval
      }
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
