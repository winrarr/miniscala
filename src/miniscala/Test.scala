package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker.{TypeEnv, TypeError, typeCheck}
import miniscala.parser.Parser.parse

object Test49 {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    testFail("{ def f(x: Int): Int = x; f(True) }")
    test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1); isEven(17) }", BoolVal(false), BoolType())
    test("{ val x = 9; val zero = 0; def sqrtIter(y: Float, i: Int): Float = if (i == zero) y else sqrtIter(improve(y), i - 1); def improve(y: Float): Float = (y + x / y) / 2; sqrtIter(1.0f, 4) }", FloatVal(3.0000916f), FloatType())
    test("{ def fib(n: Int): Int = if (n <= 1) n else fib(n - 1) + fib(n - 2); fib(8) }", IntVal(21), IntType())
    test("{ val inc = (x: Int) => x + 1; inc(42) }", IntVal(43), IntType())
    test("{ val x = (f: Int => Int) => (x: Int) => f(f(x)); def g(a: Int): Int = a + 1; x(g)(2) }", IntVal(4), IntType())
    test("{ val x = (f: Int => Boolean) => (x: Int) => f(x); def g(a: Int): Boolean = if (a < 10) true else false; x(g)(2) }", BoolVal(true), BoolType())
    testFail("{ val x = (f: Int => Boolean) => (x: Boolean) => f(x); def g(a: Int): Boolean = if (a < 10) true else false; x(g)(2) }")
    testFail("{ val x = (f: Int => Boolean) => (x: Int) => f(x); def g(a: Int): Int = if (a < 10) true else false; x(g)(2) }")
    testFail("{ val x = (f: Int => Boolean) => (x: Int) => f(x); def g(a: Int): Boolean = if (a < 10) 1 else 2; x(g)(2) }")
  }

  def test(prg: String, rval: Val, rtype: Type) = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String) = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()) = {
    val hej = parse(prg)
    val hej2 = eval(hej, env)
    assert(eval(parse(prg), env) == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()) = {
    val hej = parse(prg)
    val hej2 = typeCheck(hej, tenv)
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String) = {
    try {
      eval(parse(prg), Map[Id, Val]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}