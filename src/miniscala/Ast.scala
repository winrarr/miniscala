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
    * Identifiers are just strings.
    */
  type Var = String

  /**
    * Expressions.
    */
  sealed abstract class Exp extends AstNode

  case class VarExp(x: Var) extends Exp

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp

  case class IntLit(c: Int) extends Exp

  case class BlockExp(vals: List[ValDecl], exp: Exp) extends Exp

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
    * Declarations.
    */
  sealed abstract class Decl extends AstNode

  case class ValDecl(x: Var, exp: Exp) extends Decl

  /**
    * Exception with a message and (optionally) a source code position.
    */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
