package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse
import miniscala.Unparser.argsAsString

import scala.io.StdIn
import scala.util.parsing.input.Position

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class BoolVal(v: Boolean) extends Val
  case class FloatVal(v: Float) extends Val
  case class StringVal(v: String) extends Val
  case class TupleVal(vs: List[Val]) extends Val
  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, cenv: ClassEnv, defs: List[DefDecl]) extends Val
  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val
  case class ObjectVal(members: Env) extends Val

  val unitVal: Val = TupleVal(Nil)

  case class Constructor(params: List[FunParam], body: BlockExp, env: Env, cenv: ClassEnv, classes: List[ClassDecl], srcpos: Position)

  case class DynamicClassType(srcpos: Position) extends Type

  type Env = Map[Id, Val]

  type ClassEnv = Map[Id, Constructor]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  def eval(e: Exp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case NullLit() => (unitVal, sto)
    case VarExp(x) =>
      (getValue(env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)), sto, e), sto)
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, cenv, sto)
      val (rightval, sto2) = eval(rightexp, env, cenv, sto1)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 + v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (StringVal(v1), FloatVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (IntVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case (FloatVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 - v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 - v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 * v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 * v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '*', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 / v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 / v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 % v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 % v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '%', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (BoolVal(v1.equals(v2)), sto2)
            case (TupleVal(vs1), TupleVal(vs2)) =>
              if (vs1.length != vs2.length) return (BoolVal(false), sto2)
              for (i <- 0 to vs1.length) {
                if (vs1(i) != vs2(i)) return (BoolVal(false), sto2)
              }
              (BoolVal(true), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '==', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '<', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '<=', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (if (v1 > v2) IntVal(v1) else IntVal(v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (if (v1 > v2) FloatVal(v1) else FloatVal(v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (if (v1 > v2) FloatVal(v1) else FloatVal(v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (if (v1 > v2) FloatVal(v1) else FloatVal(v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at 'max', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 && v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '&&', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 || v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '||', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => (IntVal(-v), sto1)
            case FloatVal(v) => (FloatVal(-v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => (BoolVal(!v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val cexp = eval(condexp, env, cenv, sto)
      cexp match {
        case (BoolVal(v), sto1) => if (v) eval(thenexp, env, cenv, sto1) else eval(elseexp, env, cenv, sto1)
        case _ => throw new InterpreterError(s"Type mismatch at 'ifThenElse', unexpected value ${valueToString(cexp._1)}", condexp)
      }
    case b: BlockExp =>
      val (res, _, sto1) = evalBlock(b, env, cenv, sto)
      (res, sto1)
    case TupleExp(exps) =>
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, cenv, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              var env1 = env
              for (i <- vs.indices) {
                env1 = env1 + (c.pattern(i) -> vs(i))
              }
              return eval(c.exp, env1, cenv, sto1)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      trace(s">> Executing function ${unparse(funexp)} with arguments ${argsAsString(args)} <<\n   ${unparse(CallExp(funexp, args))}\n")
      val (closureVal, sto1) = eval(funexp, env, cenv, sto)
      closureVal match {
        case ClosureVal(params, optrestype, body, env1, cenv, defs) =>
          if (args.length != params.length)
            throw new InterpreterError(s"Unexpected amount of parameters for function '$funexp'", e)
          var env2 = env1
          for (i <- args.indices) {
            val (arg, sto2) = eval(args(i), env, cenv, sto1)
            checkValueType(arg, params(i).opttype, params(i))
            env2 += (params(i).x -> arg)
          }
          for (f <- defs) {
            env2 += (f.fun -> ClosureVal(f.params, f.optrestype, f.body, env1, cenv, defs))
          }
          val res = eval(body, env2, cenv, sto1)
          checkValueType(res._1, optrestype, e)
          res
        case _ => throw new InterpreterError(s"Unknown function '$funexp'", e)
      }
    case LambdaExp(params, body) =>
      (ClosureVal(params, None, body, env, cenv, List()), sto)
    case AssignmentExp(x, exp) =>
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, opttype) =>
          val (v, sto1) = eval(exp, env, cenv, sto)
          checkValueType(v, opttype, e)
          (unitVal, sto1 + (loc -> v))
        case _ => throw new InterpreterError("Reassignment to val", e)
      }
    case WhileExp(cond, body) =>
      var sto2 = sto
      var looping = true
      while (looping) {
        val (cexp, sto1) = eval(cond, env, cenv, sto2)
        sto2 = sto1
        cexp match {
          case BoolVal(x) => if (x) sto2 = eval(body, env, cenv, sto2)._2 else looping = false
          case _ => throw new InterpreterError("Expected boolean value in while condition", e)
        }
      }
      (unitVal, sto2)

    case NewObjExp(klass, args) =>
      trace(s">> Instantiating object '$klass' with arguments ${argsAsString(args)} <<\n   ${unparse(NewObjExp(klass, args))}\n")
      val c = cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class name '$klass'", e))
      val declcenv1 = rebindClasses(c.env, c.cenv, c.classes)
      val (declenv1, sto1) = evalArgs(args, c.params, env, sto, cenv, c.env, declcenv1, e)
      val (_, env1, sto2) = evalBlock(c.body, declenv1, declcenv1, sto1)
      val newloc = nextLoc(sto2)
      val objenv = (c.body.defs.map(d => (d.fun -> env1(d.fun))) ++ c.body.vars.map(d => (d.x -> env1(d.x))) ++ c.body.vals.map(d => (d.x -> env1(d.x)))).toMap
      val sto3 = sto2 + (newloc -> ObjectVal(objenv))
      (RefVal(newloc, Some(DynamicClassType(c.srcpos))), sto3)
    case LookupExp(objexp, member) =>
      trace(s">> Looking up field $member from object ${unparse(objexp)} <<")
      val (objval, sto1) = eval(objexp, env, cenv, sto)
      objval match {
        case RefVal(loc, _) =>
          sto1(loc) match {
            case ObjectVal(members) =>
              (getValue(members.getOrElse(member, throw new InterpreterError(s"No such member: $member", e)), sto1, e), sto1)
            case v => throw new InterpreterError(s"Base value of lookup is not a reference to an object: ${valueToString(v)}", e)
          }
        case _ => throw new InterpreterError(s"Base value of lookup is not a location: ${valueToString(objval)}", e)
      }
  }

  /**
    * Evaluates the given block.
    * Returns the resulting value, the updated environment after evaluating all declarations, and the latest store.
    */
  def evalBlock(b: BlockExp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Env, Sto) = {
    trace(s">> Evaluating following block <<\n   ${unparse(b, "   ")}\n")
    var env1 = env
    var sto1 = sto
    var cenv1 = cenv
    for (d <- b.vals) {
      val (v, sto2) = eval(d.exp, env1, cenv, sto1)
      val ot = getType(d.opttype, cenv)
      checkValueType(v, ot, b)
      sto1 = sto2
      env1 = env1 + (d.x -> v)
    }
    for (d <- b.vars) {
      val (v, sto2) = eval(d.exp, env1, cenv, sto1)
      val ot = getType(d.opttype, cenv)
      checkValueType(v, ot, b)
      val loc = nextLoc(sto)
      sto1 = sto2 + (loc -> v)
      env1 = env1 + (d.x -> RefVal(loc, ot))
    }
    for (d <- b.defs)
      env1 += (d.fun -> ClosureVal(d.params, d.optrestype, d.body, env1, cenv, b.defs))
    for (d <- b.classes)
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env1, cenv, b.classes, d.pos))
    var res: Val = unitVal
    for (exp <- b.exps) {
      val (res1, sto2) = eval(exp, env1, cenv1, sto1)
      res = res1
      sto1 = sto2
    }
    (res, env1, sto1)
  }

  /**
    * Evaluates the arguments `args` in environment `env` with store `sto`,
    * extends the environment `declenv` with the new bindings, and
    * returns the extended environment and the latest store.
    */
  def evalArgs(args: List[Exp], params: List[FunParam], env: Env, sto: Sto, cenv: ClassEnv, declenv: Env, declcenv: ClassEnv, e: Exp): (Env, Sto) = {
    trace(s">> Evaluating arguments ${argsAsString(args)} <<")
    if (args.length != params.length) throw new InterpreterError("Wrong number of arguments at call/new", e)
    var (env1, sto1) = (declenv, sto)
    for ((p, arg) <- params.zip(args) ) {
      val (argval, sto2) = eval(arg, env, cenv, sto1)
      checkValueType(argval, getType(p.opttype, declcenv), arg)
      env1 = env1 + (p.x -> argval)
      sto1 = sto2
    }
    (env1, sto1)
  }

  /**
    * If `v` is a reference to an object or it is a non-reference value, then return `v` itself;
    * otherwise, it must be a reference to a non-object value, so return that value.
    */
  def getValue(v: Val, sto: Sto, a: AstNode): Val = {
    trace(s">> Returning value of ${unparse(a)} <<")
    v match {
      case RefVal(loc, _) =>
        sto(loc) match {
          case _: ObjectVal => v
          case stoval => stoval
        }
      case _ => v
    }
  }

  /**
    * Rebinds `classes` in `cenv` to support recursive class declarations.
    */
  def rebindClasses(env: Env, cenv: ClassEnv, classes: List[ClassDecl]): ClassEnv = {
    trace(">> Rebinding classes <<")
    var cenv1 = cenv
    for (d <- classes)
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env, cenv, classes, d.pos))
    cenv1
  }

  /**
    * Returns the type described by the type annotation `ot` (if present).
    * Class names are converted to proper types according to the class environment `cenv`.
    */
  def getType(ot: Option[Type], cenv: ClassEnv): Option[Type] = ot.map(t => {
    def getType(t: Type): Type = t match {
      case ClassNameType(klass) => DynamicClassType(cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class '$klass'", t)).srcpos)
      case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
      case TupleType(ts) => TupleType(ts.map(getType))
      case FunType(paramtypes, restype) => FunType(paramtypes.map(getType), getType(restype))
      case _ => throw new RuntimeException(s"Unexpected type $t") // this case is unreachable
    }
    getType(t)
  })

  /**
    * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
    */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, optcrestype, _, _, cenv, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, getType(p.opttype, cenv), n)
          checkTypesEqual(restype, getType(optcrestype, cenv), n)
        case (RefVal(_, Some(vd: DynamicClassType)), td: DynamicClassType) =>
          if (vd != td)
            throw new InterpreterError(s"Type mismatch: object of type ${unparse(vd)} does not match type ${unparse(td)}", n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match type ${unparse(t2)}", n)
    case None => // do nothing
  }

  /**
    * Converts a value to its string representation (for error messages).
    */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(valueToString).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _, _) => // the resulting string ignores the result type annotation and the declaration environments
      s"<(${params.map(p => unparse(p)).mkString(",")}), ${unparse(exp)}>"
    case RefVal(loc, _) => s"#$loc" // the resulting string ignores the type annotation
    case ObjectVal(_) => "object" // (unreachable case)
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
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
