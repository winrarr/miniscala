package miniscala

import miniscala.Ast.Var
import miniscala.Interpreter._
import miniscala.parser.Parser.parse

object Test37 {

  def main(args: Array[String]): Unit = {
    test("true", BoolVal(true))
    test("2.0f", FloatVal(2f))
    test("\"foo\"", StringVal("foo"))
    test("5 - 2", IntVal(3))
    test("5.0f - 1.9f", FloatVal(3.1f))
    test("5 - 2.3f", FloatVal(2.7f))
    test("5.1f - 2.0f", FloatVal(3.1f))
    testFail("\"foo\" - \"bar\"")
    test("5 * 2", IntVal(10))
    test("5.0f * 2.0f", FloatVal(10f))
    test("5 * 2.1f", FloatVal(10.5f))
    test("5.1f * 2", FloatVal(10.2f))
    testFail("\"foo\" * \"bar\"")
    testFail("10 / 0")
    testFail("10 / 0.0f")
    test("10 / 5", IntVal(2))
    test("10.0f / 2.0f", FloatVal(5f))
    test("10 / 2.0f", FloatVal(5f))
    test("10.2f / 2", FloatVal(5.1f))
    testFail("\"foo\" / \"bar\"")
    testFail("10 % 0")
    testFail("10 % 0.0f")
    test("10 % 3", IntVal(1))
    test("10.0f % 2.0f", FloatVal(0f))
    test("10 % 3.0f", FloatVal(1f))
    test("10.5f % 10", FloatVal(0.5f))
    testFail("\"foo\" % \"bar\"")
    test("1 == 2.5f", BoolVal(false))
    test("2.0f == 2", BoolVal(true))
    test("1 == 1", BoolVal(true))
    test("\"foo\" == \"bar\"", BoolVal(false))
    test("2 < 5", BoolVal(true))
    test("2.0f < 2.1f", BoolVal(true))
    test("2 < 2.1f", BoolVal(true))
    test("1.9f < 2", BoolVal(true))
    testFail("\"foo\" < \"bar\"")
    test("2 <= 5", BoolVal(true))
    test("2.0f <= 2.0f", BoolVal(true))
    test("2 <= 1.9f", BoolVal(false))
    test("2.0f <= 2", BoolVal(true))
    testFail("\"foo\" <= \"bar\"")
    test("2 max 5", IntVal(5))
    test("2.0f max 2.0f", FloatVal(2f))
    test("2 max 1.9f", FloatVal(2f))
    test("2.0f max 2", FloatVal(2f))
    testFail("\"foo\" max \"bar\"")
    test("true & true", BoolVal(true))
    test("false & true", BoolVal(false))
    testFail("2 & 5")
    test("true | false", BoolVal(true))
    test("false | false", BoolVal(false))
    testFail("2 | \"foo\"")
    test("!true", BoolVal(false))
    test("!false", BoolVal(true))
    test("if (true) 1 else 2", IntVal(1))
    test("if (false) true else false", BoolVal(false))
    test("(1, 2) match { case (a,b) => a }", IntVal(1))
    test("(1, 2) match { case (a,b) => b }", IntVal(2))
    test("(1, 2, 3, 4) match { case (a,b,c,d) => d }", IntVal(4))
    test("(1, 2, 3) match { case (a,b) => a; case (a,b,c) => c; case (a,b,c,d) => d }", IntVal(3))
    test("{ val x = 1; val y = 2; x + y }", IntVal(3))
    test("{ val x: Int = 1; 1 }", IntVal(1))
    testFail("{ val x: Int = true; x }")

    // exercise 31
    val env2 = Map("x" -> IntVal(1), "y" -> TupleVal(List(IntVal(2), IntVal(3))))
    test("{ val z = 1; if (z == x) 42 else 17 }", IntVal(42), env2)
    test("y match { case (a, b) => x <= (a + b)}", BoolVal(true), env2)
    test("{ val x = (17, x == 2); y match { case (a, b) => { val z = (a + b, true); if (x == z) a - b else a + b } } }", IntVal(5), env2)
  }

  def test(prg: String, out: Val, env: VarEnv = Map[Var, Val]()) = {
    assert(eval(parse(prg), env) == out)
  }

  def testFail(prg: String) = {
    try {
      eval(parse(prg), Map[Var, Val]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
      case _: Throwable => assert(false)
    }
  }
}
