package miniscala.parser

import java.io.{File, IOException}

import miniscala.Ast._
import Tokens._

import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input._

/**
  * Parser for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Parser extends PackratParsers {

  override type Elem = MiniScalaToken

  private lazy val prog: PackratParser[Exp] = phrase { expr() }

  private def expr(antiPrecedence: Int = 2): PackratParser[Exp] =
    antiPrecedence match {
      case x if x >= 0 =>
        binopexp(antiPrecedence) |
          expr(x - 1)
      case -1 =>
        unopexp |
          expr(-2)
      case -2 =>
        literal |
          parens
    }

  private lazy val parens: PackratParser[Exp] =
    (LEFT_PAREN() ~ expr() ~ RIGHT_PAREN()) ^^ { case _ ~ exp ~ _ => exp }

  private def binopexp(antiPrecedence: Int): PackratParser[Exp] =
    expr(antiPrecedence - 1) * {
      binop(antiPrecedence) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right).setPos(left.pos) } }
    }

  private lazy val literal: PackratParser[IntLit] = positioned {
    intliteral ^^ { lit => IntLit(lit.i) }
  }

  private lazy val unopexp: PackratParser[Exp] = positioned {
    (unop ~ expr(-1)) ^^ { case op ~ exp => UnOpExp(op, exp) }
  }

  private def binop(antiPrecedence: Int): PackratParser[BinOp] = positioned {
    antiPrecedence match {
      case 0 =>
        mult | div | modulo
      case 1 =>
        plus | minus
      case 2 =>
        max
    }
  }

  private lazy val unop: PackratParser[UnOp] = positioned {
    neg
  }

  private lazy val intliteral: PackratParser[INT] = accept("int literal", { case lit: INT => lit })

  private lazy val plus: PackratParser[BinOp] = OP("+") ^^ { _ => PlusBinOp() }

  private lazy val minus: PackratParser[BinOp] = OP("-") ^^ { _ => MinusBinOp() }

  private lazy val mult: PackratParser[BinOp] = OP("*") ^^ { _ => MultBinOp() }

  private lazy val div: PackratParser[BinOp] = OP("/") ^^ { _ => DivBinOp() }

  private lazy val max: PackratParser[BinOp] = OP("max") ^^ { _ => MaxBinOp() }

  private lazy val modulo: PackratParser[BinOp] = OP("%") ^^ { _ => ModuloBinOp() }

  private lazy val neg: PackratParser[UnOp] = OP("-") ^^ { _ => NegUnOp() }

  private def parseTokens(tokens: Seq[MiniScalaToken]): Exp =
    prog(new MiniScalaTokenReader(tokens)) match {
      case NoSuccess(_, next) =>
        throw new SyntaxError(next.pos)
      case Success(result, next) =>
        result
    }

  def parse(code: String): Exp =
    parseTokens(Lexer(code))

  def readFile(path: String): String = {
    try {
      val f = Source.fromFile(new File(path))
      try {
        f.mkString
      } finally {
        f.close()
      }
    } catch {
      case e: IOException =>
        throw new MiniScalaError(s"Unable to read file ${e.getMessage}", NoPosition)
    }
  }

  class SyntaxError(pos: Position) extends MiniScalaError(s"Syntax error", pos)

  class MiniScalaTokenReader(tokens: Seq[MiniScalaToken]) extends Reader[MiniScalaToken] {

    override def first: MiniScalaToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[MiniScalaToken] = new MiniScalaTokenReader(tokens.tail)
  }
}
