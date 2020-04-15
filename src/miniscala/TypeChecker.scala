package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  case class RefType(thetype: Type) extends Type

  val unitType = TupleType(Nil)

  def typeCheck(e: Exp, tenv: TypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) => tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)) match {
      case RefType(thetype) => thetype
      case t: Type => t
    }
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv)
      val righttype = typeCheck(rightexp, tenv)
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
      val exptype = typeCheck(exp, tenv)
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
      val condtype = typeCheck(condexp, tenv)
      val thentype = typeCheck(thenexp, tenv)
      val elsetype = typeCheck(elseexp, tenv)
      (condtype, thentype, elsetype) match {
        case (BoolType(), a, b) =>
          if (a == b) a else
            throw new TypeError(s"Type mismatch at 'if-else' expressions, unexpected types $thenexp) and $elseexp)", EqualBinOp())
        case _ => throw new TypeError(s"Type mismatch at 'if-else' condition, unexpected type ${unparse(condtype)}, expected Bool", EqualBinOp())
      }
    case BlockExp(vals, vars, defs, exps) =>
      var tenv1 = tenv
      for (v <- vals) {
        val t = typeCheck(v.exp, tenv1)
        checkTypesEqual(t, v.opttype, v)
        tenv1 = tenv1 + (v.x -> v.opttype.getOrElse(t))
      }
      for (v <- vars) {
        val t = typeCheck(v.exp, tenv1)
        checkTypesEqual(t, v.opttype, v)
        tenv1 = tenv1 + (v.x -> RefType(v.opttype.getOrElse(t)))
      }
      for (d <- defs) {
        tenv1 = tenv1 + (d.fun -> getFunType(d))
      }
      for (d <- defs) {
        val argsenv = d.params.map(f => f.x -> f.opttype.getOrElse(throw new TypeError(s"Unexpected type for parameter '${f.x}'", e)))
        checkTypesEqual(typeCheck(d.body, tenv1 ++ argsenv), d.optrestype, d)
      }
      var v: Type = unitType
      for (exp <- exps) {
        v = typeCheck(exp, tenv1)
      }
      v
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, tenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              typeCheck(c.exp, tenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val closureType = typeCheck(funexp, tenv)
      closureType match {
        case FunType(paramtypes, restype) =>
          if (paramtypes.length != args.length)
            throw new TypeError(s"Unexpected amount of parameters for function '$funexp'", e)
          for (i <- args.indices) {
            if (typeCheck(args(i), tenv) != paramtypes(i))
              throw new TypeError(s"Return type mismatch for function '$funexp'", e)
          }
          restype
        case _ => throw new TypeError(s"Unknown function '$funexp'", e)
      }
    case LambdaExp(params, body) =>
      val paramTypes = params.map(p => p.x -> p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p)))
      FunType(paramTypes.map(p => p._2), typeCheck(body, tenv ++ paramTypes))
    case AssignmentExp(x, exp) =>
      val h = tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
       h match {
        case RefType(x) =>
          checkTypesEqual(x, Option(typeCheck(exp, tenv)), e)
          unitType
        case _ => throw new TypeError("Reassignment to val", e)
      }
    case WhileExp(cond, body) =>
      checkTypesEqual(typeCheck(cond, tenv), Option(BoolType()), e)
      typeCheck(body, tenv)
      unitType
  }

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
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
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Vars.freeVars(program))
      tenv = tenv + (x -> IntType())
    tenv
  }

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
