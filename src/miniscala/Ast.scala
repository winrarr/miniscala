package miniscala

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
  * Abstract syntax tree representation of MiniScala programs.
  */
object Ast {

  /**
    * An AST node contains information about its position in the source code.
    */
  sealed abstract class AstNode extends Positional

  /**
    * Expressions.
    */
  sealed abstract class Exp extends AstNode

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp

  case class IntLit(c: Int) extends Exp

  /**
    * Binary operators.
    */
  sealed abstract class BinOp extends AstNode

  case class PlusBinOp() extends BinOp

  case class MinusBinOp() extends BinOp

  case class MultBinOp() extends BinOp

  case class DivBinOp() extends BinOp

  case class ModuloBinOp() extends BinOp

  case class MaxBinOp() extends BinOp

  /**
    * Unary operators.
    */
  sealed abstract class UnOp extends AstNode

  case class NegUnOp() extends UnOp

  /**
    * Exception with a message and (optionally) a source code position.
    */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
