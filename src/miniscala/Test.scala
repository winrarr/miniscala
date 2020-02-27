package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker.{FunTypeEnv, TypeError, VarTypeEnv, typeCheck}
import miniscala.parser.Parser.parse

object Test49 {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    testFail("{ def f(x: Int): Int = x; f(True) }")
    test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1); isEven(17) }", BoolVal(false), BoolType())
    test("{ val x = 9; val zero = 0; def sqrtIter(y: Float, i: Int): Float = if (i == zero) y else sqrtIter(improve(y), i - 1); def improve(y: Float): Float = (y + x / y) / 2; sqrtIter(1.0f, 4) }", FloatVal(3.0000916f), FloatType())
    test("{ def fib(n: Int): Int = if (n <= 1) n else fib(n - 1) + fib(n - 2); fib(8) }", IntVal(21), IntType())
  }

  def test(prg: String, rval: Val, rtype: Type) = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String) = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, venv: VarEnv = Map[Var, Val](), fenv: FunEnv = Map[Var, Closure]()) = {
    assert(eval(parse(prg), venv, fenv) == value)
  }

  def testType(prg: String, out: Type, venv: VarTypeEnv = Map[Var, Type](), fenv: FunTypeEnv = Map[Var, (List[Type], Type)]()) = {
    assert(typeCheck(parse(prg), venv, fenv) == out)
  }

  def testValFail(prg: String) = {
    try {
      eval(parse(prg), Map[Var, Val](), Map[Var, Closure]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map[Var, Type](), Map[Var, (List[Type], Type)]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}