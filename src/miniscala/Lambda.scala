package miniscala

import miniscala.Ast._
import miniscala.parser.Parser
import miniscala.Interpreter._

import scala.io.StdIn

/**
  * Compiler from (a large part of) MiniScala v5 to lambda calculus.
  */
object Lambda {

  val FIX: Exp = Parser.parse("((x)=>(y)=>(y((z)=>x(x)(y)(z))))((x)=>(y)=>(y((z)=>x(x)(y)(z))))")

  def encode(e: Exp): Exp =
    e match {
      case IntLit(c) =>
        if (c == 0) LambdaExp(List(FunParam("s",None)), LambdaExp(List(FunParam("z", None)), VarExp("z")))
        else LambdaExp(List(FunParam("s",None)), LambdaExp(List(FunParam("z", None)), CallExp(encode(VarExp("s")), List(VarExp("z")))))
      case BoolLit(c) => // boolean literals are encoded as on slide 15
        if (c) LambdaExp(List(FunParam("t",None)), LambdaExp(List(FunParam("e", None)), VarExp("t")))
        else LambdaExp(List(FunParam("t", None)), LambdaExp(List(FunParam("e", None)), VarExp("e")))
      case VarExp(id) => // variables need no encoding
        e
      case BinOpExp(leftexp, EqualBinOp(), IntLit(0)) => // e == 0, slide 18
        ???
      case BinOpExp(leftexp, MinusBinOp(), IntLit(1)) => // e - 1, slide 20
        LambdaExp(List(FunParam("s", None)), LambdaExp(List(FunParam("z", None)),
          CallExp(CallExp(CallExp(encode(leftexp), List(
            LambdaExp(List(FunParam("g", None)), LambdaExp(List(FunParam("h", None)),
              CallExp(VarExp("h"), List(CallExp(VarExp("g"), List(VarExp("s"))))))))),
            List(LambdaExp(List(FunParam("u", None)), VarExp("z")))),
            List(LambdaExp(List(FunParam("u", None)), VarExp("u"))))))
      case BinOpExp(leftexp, op, rightexp) =>
        op match {
          case PlusBinOp() => // e1 + e2, slide 20 (we assume there are no floats or strings)
            LambdaExp(List(FunParam("s", None)), LambdaExp(List(FunParam("z", None)),
              CallExp(CallExp(encode(leftexp), List(VarExp("s"))),
                List(CallExp(CallExp(encode(rightexp), List(VarExp("s"))), List(VarExp("z")))))))
          case MultBinOp() => // e1 * e2, slide 20
            LambdaExp(List(FunParam("s", None)), CallExp(encode(rightexp), List(CallExp(encode(leftexp), List(VarExp("s"))))))
          case AndBinOp() => // e1 & e2, slide 15
            encode(IfThenElseExp(leftexp, rightexp, BoolLit(false)))
          case OrBinOp() => // e1 | e2, slide 15
            encode(IfThenElseExp(leftexp, BoolLit(true), rightexp))
          case MinusBinOp() => // e1 - e2 (not in slides, see https://en.wikipedia.org/wiki/Church_encoding)
            CallExp(CallExp(encode(rightexp),
              List(LambdaExp(List(FunParam("n", None)),
                LambdaExp(List(FunParam("s", None)), LambdaExp(List(FunParam("z", None)),
                  CallExp(CallExp(CallExp(VarExp("n"), List(
                    LambdaExp(List(FunParam("g", None)), LambdaExp(List(FunParam("h", None)),
                      CallExp(VarExp("h"), List(CallExp(VarExp("g"), List(VarExp("s"))))))))),
                    List(LambdaExp(List(FunParam("u", None)), VarExp("z")))),
                    List(LambdaExp(List(FunParam("u", None)), VarExp("u"))))))))),
              List(encode(leftexp)))
          case LessThanOrEqualBinOp() => // e1 <= e2 (not in slides, see https://en.wikipedia.org/wiki/Church_encoding)
            encode(BinOpExp(BinOpExp(leftexp, MinusBinOp(), rightexp), EqualBinOp(), IntLit(0)))
          case LessThanBinOp() => // e1 < e2 (not in slides)
            encode(BinOpExp(BinOpExp(BinOpExp(leftexp, PlusBinOp(), IntLit(1)), MinusBinOp(), rightexp), EqualBinOp(), IntLit(0)))
          case _ => // remaining cases are not (yet) implemented
            throw new EncoderError(e)
        }
      case UnOpExp(op, subexp) =>
        op match {
          case NotUnOp() => // !e, slide 15
            ???
          case _ => // remaining cases are not (yet) implemented
            throw new EncoderError(e)
        }
      case IfThenElseExp(condexp, thenexp, elseexp) => // if (e1) e2 else e3, slide 15
        // CallExp(CallExp(encode(condexp), List(encode(thenexp))), List(encode(elseexp))) // no good, evaluates both branches if using call-by-value
        CallExp(CallExp(encode(condexp),
          List(LambdaExp(List(FunParam("a", None)), CallExp(encode(thenexp), List(VarExp("a")))))),
          List(LambdaExp(List(FunParam("b", None)), CallExp(encode(elseexp), List(VarExp("b")))))) // mimics call-by-name
      case BlockExp(List(ValDecl(id, _, e1)), List(), e2: Exp) => // { val x = e1; e2 }, slide 23
        ???
      case BlockExp(List(), List(DefDecl(f, List(FunParam(x, _)), _, e1)), e2: Exp) => // { def f(x) = e1; e2 }, slide 23
        CallExp(LambdaExp(List(FunParam(f, None)), encode(e2)),
          List(CallExp(FIX,
            List(LambdaExp(List(FunParam(f, None)), LambdaExp(List(FunParam(x, None)), encode(e1)))))))
      case TupleExp(List(e1, e2)) => // (e1, e2), slide 21
        LambdaExp(List(FunParam("p", None)),
          CallExp(CallExp(VarExp("p"), List(encode(e1))), List(encode(e2))))
      case MatchExp(mexp, List(MatchCase(List(x, y), caseexp))) => // e1 match { case (x,y) => e2 }, slide 21
        encode(BlockExp(List(ValDecl("p", None, mexp)), List(),
          BlockExp(List(ValDecl(x, None, CallExp(VarExp("p"), List(LambdaExp(List(FunParam("x", None)), LambdaExp(List(FunParam("y", None)), VarExp("x"))))))), List(),
            BlockExp(List(ValDecl(y, None, CallExp(VarExp("p"), List(LambdaExp(List(FunParam("x", None)), LambdaExp(List(FunParam("y", None)), VarExp("y"))))))), List(),
              caseexp))))
      case CallExp(target, args) => // call expressions are trivial, just encode the arguments recursively
        CallExp(encode(target), args.foldLeft(List[Exp]())((es, a) => encode(a) :: es))
      case LambdaExp(params, body) => // lambdas are trivial, just encode the body recursively
        val ps = params.map(p => FunParam(p.x, None)) // remove the type annotations, to avoid annoying the dynamic type checker
        LambdaExp(ps, encode(body))
      case _ => // remaining cases are not (yet) implemented
        throw new EncoderError(e)
    }

  def decodeNumber(v: Val): Int = v match {
    case ClosureVal(params, _, exp, env) =>
      val unchurch = // see slide 22
        CallExp(CallExp(LambdaExp(params, exp),
          List(LambdaExp(List(FunParam("n", None)), BinOpExp(VarExp("n"), PlusBinOp(), IntLit(1))))),
          List(IntLit(0)))
      Interpreter.eval(unchurch, env) match {
        case IntVal(c) => c
        case _ => throw new RuntimeException(s"Unexpected decoded value $v")
      }
    case _ => throw new RuntimeException(s"Unexpected encoded value $v")
  }

  def decodeBoolean(v: Val): Boolean = v match {
    case ClosureVal(params, _, exp, env) =>
      val unchurch = // see slide 22
        ???
      Interpreter.eval(unchurch, env) match {
        case BoolVal(c) => c
        case _ => throw new RuntimeException(s"Unexpected decoded value $v")
      }
    case _ => throw new RuntimeException(s"Unexpected encoded value $v")
  }

  /**
    * Builds an initial environment, with a lambda-encoded value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> Interpreter.eval(encode(IntLit(StdIn.readInt())), Map[Id, Val]()))
    }
    env
  }

  class EncoderError(node: AstNode) extends MiniScalaError(s"Don't know how to encode $node", node.pos)
}
