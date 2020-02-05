package miniscala

import miniscala.Ast._

/**
  * Interpreter for MiniScala.
  */
object Interpreter {
  def eval(e: Exp): Int = e.eval()

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
