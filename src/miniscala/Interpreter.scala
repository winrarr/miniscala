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
        case PlusBinOp() => leftval + rightval
        case MinusBinOp() => ???
        case MultBinOp() => ???
        case DivBinOp() =>
          if (rightval == 0)
            throw new InterpreterError(s"Division by zero", op)
          leftval / rightval
        case ModuloBinOp() => ???
        case MaxBinOp() =>
          if (???) ??? else ???
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp, venv)
      op match {
        case NegUnOp() => -expval
      }
    case BlockExp(vals, exp) =>
      var venv1 = venv
      for (d <- vals)
        venv1 = venv1 + (d.x -> eval(d.exp, venv1))
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
