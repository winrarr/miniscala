package miniscala

import miniscala.Ast._

import scala.io.StdIn

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  type VarEnv = Map[Var, Int]

  def eval(e: Exp, venv: VarEnv): Int = e match {
    case IntLit(c) => c
    case VarExp(x) => venv(x)
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, venv)
      val rightval = eval(rightexp, venv)
      op match {
        case PlusBinOp() =>
          trace(s"Adding $leftval to $rightval")
          leftval + rightval
        case MinusBinOp() =>
          trace(s"Subtracting $rightval from $leftval")
          leftval - rightval
        case MultBinOp() =>
          trace(s"Multiplying $leftval and $rightval")
          leftval * rightval
        case DivBinOp() =>
          if (rightval == 0)
            throw new InterpreterError(s"Division by zero", op)
          trace(s"Dividing $leftval by $rightval")
          leftval / rightval
        case ModuloBinOp() =>
          trace(s"Computing $leftval mod $rightval")
          leftval % rightval
        case MaxBinOp() =>
          trace(s"Finding max value of $leftval and $rightval")
          if (leftval > rightval) leftval else rightval
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp, venv)
      op match {
        case NegUnOp() =>
          trace(s"Computing negative value of $expval")
          -expval
      }
    case BlockExp(vals, exp) =>
      trace(s"Evaluating for variable environment: $venv")
      var venv1 = venv
      for (d <- vals)
        venv1 += (d.x -> eval(d.exp, venv1))
      eval(exp, venv1)
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialVarEnv(program: Exp): VarEnv = {
    var venv = Map[Var, Int]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      venv = venv + (x -> StdIn.readInt())
    }
    venv
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
