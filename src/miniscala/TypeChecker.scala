package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type VarTypeEnv = Map[Var, Type]

  type FunTypeEnv = Map[Fun, (List[Type], Type)]

  def typeCheck(e: Exp, vtenv: VarTypeEnv, ftenv: FunTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) => vtenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, vtenv, ftenv)
      val righttype = typeCheck(rightexp, vtenv, ftenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case EqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '==', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case LessThanBinOp() | LessThanOrEqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '<', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case AndBinOp() | OrBinOp() =>
          (lefttype, righttype) match {
            case (BoolType(), BoolType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '&', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      op match {
        case NegUnOp() => exptype match {
          case IntType() => IntType()
          case FloatType() => FloatType()
          case _ => throw new TypeError(s"Type mismatch at '-', unexpected type ${unparse(exptype)}", op)
        }
        case NotUnOp() => exptype match {
          case BoolType() => BoolType()
          case _ => throw new TypeError(s"Type mismatch at '!', unexpected type ${unparse(exptype)}", op)
        }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condtype = typeCheck(condexp, vtenv, ftenv)
      val thentype = typeCheck(thenexp, vtenv, ftenv)
      val elsetype = typeCheck(elseexp, vtenv, ftenv)
      (condtype, thentype, elsetype) match {
        case (BoolType(), a, b) =>
          if (a == b) a else
//            throw new TypeError(s"Type mismatch at 'if-else' expressions, unexpected types ${unparse(thenexp)} and ${unparse(elseexp)}", EqualBinOp())
            throw new TypeError(s"Type mismatch at 'if-else' expressions, unexpected types $thenexp) and $elseexp)", EqualBinOp())
        case _ => throw new TypeError(s"Type mismatch at 'if-else' condition, unexpected type ${unparse(condtype)}, expected Bool", EqualBinOp())
      }
    case BlockExp(vals, defs, exp) =>
      var vtenv1 = vtenv
      for (d <- vals) {
        val t = typeCheck(d.exp, vtenv1, ftenv)
        checkTypesEqual(t, d.opttype, d)
        vtenv1 = vtenv1 + (d.x -> d.opttype.getOrElse(t))
      }
      var ftenv1 = ftenv
      for (d <- defs) {
        val argTypes = d.params.map(f => f.opttype.getOrElse(throw new TypeError(s"Unexpected type for parameter '${f.x}", e)))
        val resType = d.optrestype.getOrElse(throw new TypeError(s"Unexpected function type for function '${d.fun}'", e))
        ftenv1 = ftenv1 + (d.fun -> (argTypes, resType))
      }
      for (d <- defs) {
        val funcvtenv = d.params.map(f => f.x -> f.opttype.getOrElse(throw new TypeError(s"Unexpected type for parameter '${f.x}", e))).toMap
        checkTypesEqual(typeCheck(d.body, vtenv1 ++ funcvtenv, ftenv1), d.optrestype, e)
      }
      typeCheck(exp, vtenv1, ftenv1)
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, vtenv, ftenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              typeCheck(c.exp, vtenv, ftenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(fun, args) =>
      val func = ftenv.getOrElse(fun, throw new TypeError(s"Unknown function '$fun'", e))
      if (args.length != func._1.length)
        throw new TypeError(s"Unexpected amount of parameters for function '$fun'", e)
      for (i <- args.indices) {
        if (typeCheck(args(i), vtenv, ftenv) != func._1(i))
          throw new TypeError(s"Return type mismatch for function '$fun'", e)
      }
      func._2
  }

  /**
    * Returns the parameter types and return type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): (List[Type], Type) =
    (d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
    * Builds an initial type environment, with a type for each free variable in the program.
    */
  def makeInitialVarTypeEnv(program: Exp): VarTypeEnv = {
    var vtenv: VarTypeEnv = Map()
    for (x <- Vars.freeVars(program))
      vtenv = vtenv + (x -> IntType())
    vtenv
  }

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
