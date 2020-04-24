package miniscala

import miniscala.Ast._
import miniscala.Interpreter.{DynamicClassType, InterpreterError}
import miniscala.Unparser.unparse

import scala.util.parsing.input.Position

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  type ClassTypeEnv = Map[Id, StaticClassType]

  case class RefType(thetype: Type) extends Type

  case class StaticClassType(klass: Id, srcpos: Position, params: List[FunParam], membertypes: TypeEnv) extends Type

  val unitType: Type = TupleType(Nil)

  def typeCheck(e: Exp, tenv: TypeEnv, ctenv: ClassTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case NullLit() => NullType()
    case VarExp(x) => getType(tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)), ctenv)
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv, ctenv)
      val righttype = typeCheck(rightexp, tenv, ctenv)
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
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types '${unparse(lefttype)}' and '${unparse(righttype)}'", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case _ => throw new TypeError(s"Type mismatch, unexpected types '${unparse(lefttype)}' and '${unparse(righttype)}'", op)
          }
        case EqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '==', unexpected types '${unparse(lefttype)}' and '${unparse(righttype)}'", op)
          }
        case LessThanBinOp() | LessThanOrEqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '<', unexpected types '${unparse(lefttype)}' and '${unparse(righttype)}'", op)
          }
        case AndBinOp() | OrBinOp() | AndAndBinOp() | OrOrBinOp() =>
          (lefttype, righttype) match {
            case (BoolType(), BoolType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '&', unexpected types '${unparse(lefttype)}' and '${unparse(righttype)}'", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      op match {
        case NegUnOp() => exptype match {
          case IntType() => IntType()
          case FloatType() => FloatType()
          case _ => throw new TypeError(s"Type mismatch at '-', unexpected type '${unparse(exptype)}'", op)
        }
        case NotUnOp() => exptype match {
          case BoolType() => BoolType()
          case _ => throw new TypeError(s"Type mismatch at '!', unexpected type '${unparse(exptype)}'", op)
        }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condtype = typeCheck(condexp, tenv, ctenv)
      val thentype = typeCheck(thenexp, tenv, ctenv)
      val elsetype = typeCheck(elseexp, tenv, ctenv)
      (condtype, thentype, elsetype) match {
        case (BoolType(), a, b) =>
          if (a == b) a else
            throw new TypeError(s"Type mismatch at 'if-else' expressions, unexpected types '$thenexp' and '$elseexp'", EqualBinOp())
        case _ => throw new TypeError(s"Type mismatch at 'if-else' condition, unexpected type '${unparse(condtype)}', expected Bool", EqualBinOp())
      }
    case b: BlockExp =>
      typeCheckBlock(b, tenv, ctenv)._1
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, tenv, ctenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              typeCheck(c.exp, tenv, ctenv)
            }
          }
          throw new TypeError(s"No case matches type '${unparse(exptype)}'", e)
        case _ => throw new TypeError(s"Tuple expected at match, found '${unparse(exptype)}'", e)
      }
    case CallExp(funexp, args) =>
      typeCheck(funexp, tenv, ctenv) match {
        case FunType(paramtypes, restype) =>
          if (paramtypes.length != args.length)
            throw new TypeError(s"Unexpected amount of parameters for function '$funexp'", e)
          for (i <- args.indices) {
            if (typeCheck(args(i), tenv, ctenv) != getType(paramtypes(i), ctenv))
              throw new TypeError(s"Return type mismatch for function '$funexp'", e)
          }
          restype
        case _ => throw new TypeError(s"Unknown function '$funexp'", e)
      }
    case LambdaExp(params, body) =>
      val paramTypes = params.map(p => p.x -> p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p)))
      FunType(paramTypes.map(p => p._2), typeCheck(body, tenv ++ paramTypes, ctenv))
    case AssignmentExp(x, exp) =>
      val h = tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
      h match {
        case RefType(x) =>
          (x, typeCheck(exp, tenv, ctenv)) match {
            case (ClassNameType(a), StaticClassType(b, _, _, _)) =>
              if (a != b) {
                throw new TypeError(s"Assignment of type '$b' to variable of type '$a'", e)
              }
              unitType
            case _ =>
              checkSubtype(x, typeCheck(exp, tenv, ctenv), e)
              unitType
          }
        case _ => throw new TypeError("Reassignment to val", e)
      }
    case WhileExp(cond, body) =>
      checkSubtype(typeCheck(cond, tenv, ctenv), BoolType(), e)
      typeCheck(body, tenv, ctenv)
      unitType
    case DoWhileExp(body, cond) =>
      checkSubtype(typeCheck(cond, tenv, ctenv), BoolType(), e)
      typeCheck(body, tenv, ctenv)
      unitType
    case NewObjExp(klass, args) =>
      ctenv.getOrElse(klass, throw new TypeError(s"Unknown class name '$klass'", e)) match {
        case b: StaticClassType =>
          val ctenv1 = rebindClasses(ctenv)
          if (args.length != b.params.length)
            throw new TypeError(s"Unexpected amount of parameters for class '$klass'", e)
          for (i <- args.indices) {
            checkSubtype(typeCheck(args(i), tenv, ctenv1), getType(b.params(i).opttype, ctenv1), b)
          }
          b
      }
    case LookupExp(objexp, member) =>
      typeCheck(objexp, tenv, ctenv) match {
        case StaticClassType(_, _, _, membertypes) =>
          membertypes.getOrElse(member, throw new TypeError(s"Unknown member '$member' from object '${unparse(objexp)}'", e))
        case _ => throw new TypeError(s"Unknown object '${unparse(objexp)}'", e)
      }
  }

  def typeCheckBlock(b: BlockExp, tenv: TypeEnv, ctenv: ClassTypeEnv): (Type, TypeEnv) = {
    var tenv1 = tenv
    var ctenv1 = ctenv

    // vals
    for (v <- b.vals) {
      val t = typeCheck(v.exp, tenv1, ctenv)
      checkSubtype(t, getType(v.opttype, ctenv), v)
      tenv1 = tenv1 + (v.x -> getType(v.opttype.getOrElse(t), ctenv))
    }

    // vars
    for (v <- b.vars) {
      val t = typeCheck(v.exp, tenv1, ctenv)
      checkSubtype(t, getType(v.opttype, ctenv), v)
      tenv1 = tenv1 + (v.x -> RefType(getType(v.opttype.getOrElse(t), ctenv)))
    }

    // defs
    for (d <- b.defs) {
      tenv1 = tenv1 + (d.fun -> getFunType(d))
    }
    for (d <- b.defs) {
      val argsenv = d.params.map(f => f.x -> f.opttype.getOrElse(throw new TypeError(s"Unexpected type for parameter '${f.x}'", b)))
      checkSubtype(typeCheck(d.body, tenv1 ++ argsenv, ctenv), d.optrestype, d)
    }

    // classes
    for (d <- b.classes) {
      var classInsideEnv: TypeEnv = d.params.map(p => (p.x -> p.opttype.getOrElse(throw new TypeError(s"Expected parameter type, but found none", d)))).toMap
      classInsideEnv = classInsideEnv ++ typeCheckBlock(d.body, classInsideEnv, ctenv1)._2
      ctenv1 = ctenv1 + (d.klass -> StaticClassType(d.klass, d.pos, d.params, classInsideEnv))
    }

    // exps
    var v: Type = unitType
    for (exp <- b.exps) {
      v = typeCheck(exp, tenv1, ctenv1)
    }

    (v, tenv1)
  }

  /**
    * Checks whether `t1` is a subtype of `t2`.
    */
  def subtype(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (NullType(), b: StaticClassType) => true // do nothing
    case (a: StaticClassType, b: StaticClassType) => a.srcpos == b.srcpos
    case (IntType(), FloatType()) => true
    case (t1, t2) => t1 == t2
  }

  /**
    * Checks whether `t1` is a subtype of `t2`, generates type error otherwise.
    */
  def checkSubtype(t1: Type, t2: Type, n: AstNode): Unit =
    if (!subtype(t1, t2)) throw new TypeError(s"Type mismatch: type ${unparse(t1)} is not subtype of ${unparse(t2)}", n)

  /**
    * Checks whether `t1` is a subtype of `ot2` (if present), generates type error otherwise.
    */
  def checkSubtype(t: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) => checkSubtype(t, t2, n)
    case None => // do nothing
  }

  /**
    * Returns the type described by the type annotation `t`.
    * Class names are converted to proper types according to the class type environment `ctenv`.
    */
  def getType(t: Type, ctenv: ClassTypeEnv): Type = t match {
    case RefType(x) => getType(x, ctenv)
    case ClassNameType(klass) => ctenv.getOrElse(klass, throw new TypeError(s"Unknown class '$klass'", t))
    case b: StaticClassType => b
    case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
    case TupleType(ts) => TupleType(ts.map(tt => getType(tt, ctenv)))
    case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt, ctenv)), getType(restype, ctenv))
    case _ => throw new RuntimeException(s"Unexpected type '$t'") // this case is unreachable...
  }

  /**
    * Returns the type described by the optional type annotation `ot` (if present).
    */
  def getType(ot: Option[Type], ctenv: ClassTypeEnv): Option[Type] = ot.map(t => getType(t, ctenv))

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter '${p.x}'", p))),
      d.optrestype.getOrElse(unitType))

  /**
    * Returns the class type for the class declaration `d`.
    */
  def getClassType(d: ClassDecl): StaticClassType = {
    var membertypes: TypeEnv = Map()
    for (m <- d.body.vals)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field '${m.x}'", m)))
    for (m <- d.body.vars)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field '${m.x}'", m)))
    for (m <- d.body.defs)
      membertypes = membertypes + (m.fun -> getFunType(m))
    StaticClassType(d.klass, d.pos, d.params, membertypes)
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = (t1, ot2) match {
    case (NullType(), Some(_: StaticClassType)) => // do nothing
    case (a: StaticClassType, Some(b: StaticClassType)) =>
      if (a.srcpos != b.srcpos) {
        throw new TypeError(s"Type mismatch: expected type '${b.klass} (${b.srcpos})', but found type '${a.klass} (${a.srcpos})'", n)
      }
    case (_, Some(t2)) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type '${unparse(t2)}', but found type '${unparse(t1)} $n'", n)
    case (_, None) => // do nothing
  }

  def rebindClasses(ctenv: ClassTypeEnv): ClassTypeEnv = {
    var ctenv1 = ctenv
    for (d <- ctenv) {
      ctenv1 = ctenv1 + d
    }
    ctenv1
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
