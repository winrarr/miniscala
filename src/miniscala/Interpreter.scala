package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

import scala.io.StdIn

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
  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, defs: List[DefDecl]) extends Val
  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val

  val unitVal: TupleVal = TupleVal(Nil)

  type Env = Map[Id, Val]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  def eval(e: Exp, env: Env, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case VarExp(x) =>
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, _) => (sto(loc), sto)
        case v: Val => (v, sto)
      }
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, sto)
      val (rightval, sto2) = eval(rightexp, env, sto1)
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
      val (expval, sto1) = eval(exp, env, sto)
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
      val cexp = eval(condexp, env, sto)
      cexp match {
        case (BoolVal(v), sto1) => if (v) eval(thenexp, env, sto1) else eval(elseexp, env, sto1)
        case _ => throw new InterpreterError(s"Type mismatch at 'ifThenElse', unexpected value ${valueToString(cexp._1)}", condexp)
      }
    case BlockExp(vals, vars, defs, exps) =>
      var env1 = env
      var sto1 = sto
      for (d <- vals) {
        val (v, sto2) = eval(d.exp, env1, sto1)
        checkValueType(v, d.opttype, e)
        env1 = env1 + (d.x -> v)
        sto1 = sto2
      }
      for (d <- vars) {
        val (v, sto2) = eval(d.exp, env1, sto1)
        checkValueType(v, d.opttype, e)
        val location = nextLoc(sto2)
        sto1 = sto2 + (location -> v)
        env1 = env1 + (d.x -> RefVal(location, Option(getType(v, e))))
      }
      for (d <- defs) {
        env1 += (d.fun -> ClosureVal(d.params, d.optrestype, d.body, env1, defs))
      }
      var v: (Val, Sto) = (unitVal, sto1)
      for (exp <- exps) {
        v = eval(exp, env1, v._2)
      }
      v
    case TupleExp(exps) =>
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      val (expval, sto1) = eval(exp, env, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              var env1 = env
              for (i <- vs.indices) {
                env1 = env1 + (c.pattern(i) -> vs(i))
              }
              return eval(c.exp, env1, sto1)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      val (closureVal, sto1) = eval(funexp, env, sto)
      closureVal match {
        case ClosureVal(params, optrestype, body, env1, defs) =>
          if (args.length != params.length)
            throw new InterpreterError(s"Unexpected amount of parameters for function '$funexp'", e)
          var env2 = env1
          for (i <- args.indices) {
            val (arg, sto2) = eval(args(i), env, sto1)
            checkValueType(arg, params(i).opttype, params(i))
            env2 += (params(i).x -> arg)
          }
          for (f <- defs) {
            env2 += (f.fun -> ClosureVal(f.params, f.optrestype, f.body, env1, defs))
          }
          val res = eval(body, env2, sto1)
          checkValueType(res._1, optrestype, e)
          res
        case _ => throw new InterpreterError(s"Unknown function '$funexp'", e)
      }
    case LambdaExp(params, body) =>
      (ClosureVal(params, None, body, env, List()), sto)
    case AssignmentExp(x, exp) =>
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, opttype) =>
          val (v, sto1) = eval(exp, env, sto)
          checkValueType(v, opttype, e)
          (unitVal, sto1 + (loc -> v))
        case _ => throw new InterpreterError("Reassignment to val", e)
      }
    case WhileExp(cond, body) =>
      var sto2 = sto
      var looping = true
      while (looping) {
        val (cexp, sto1) = eval(cond, env, sto2)
        sto2 = sto1
        cexp match {
          case BoolVal(x) => if (x) sto2 = eval(body, env, sto2)._2 else looping = false
          case _ => throw new InterpreterError("Expected boolean value in while condition", e)
        }
      }
      (unitVal, sto2)
  }

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
        case (ClosureVal(cparams, optcrestype, _, _, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, p.opttype, n)
          checkTypesEqual(restype, optcrestype, n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  def getType(v: Val, e: AstNode): Type = v match {
    case IntVal(_) => IntType()
    case BoolVal(_) => BoolType()
    case FloatVal(_) => FloatType()
    case StringVal(_) => StringType()
    case TupleVal(x) => TupleType(x.map(p => getType(p, e)))
    case ClosureVal(params, restype, _, _, _) => FunType(params.map(p => p.opttype.getOrElse(throw new InterpreterError("Error", e))), restype.getOrElse(throw new InterpreterError("Error", e)))
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match expected type ${unparse(t2)}", n)
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
    case ClosureVal(params, _, exp, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(unparse).mkString(",")}), ${unparse(exp)}>"
    case RefVal(loc, _) => s"#$loc" // the resulting string ignores the type annotation
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
