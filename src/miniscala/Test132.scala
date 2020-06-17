package miniscala

import miniscala.AbstractMachine._
import miniscala.Compiler._
import miniscala.parser.Parser.parse

object Test132 {

  def main(args: Array[String]): Unit = {
//    test("2 + 3", 5)
//    test("-5", -5)
//    test("5 - 2", 3)
//    test("2 * 5", 10)
//    test("10 / 2 * 3", 15)
//    test("true & false", 0)
//    test("false | true", 1)
//    test("3 * 2 < 20", 1)
//    test("3 == 3", 1)
//    test("4 * 2 <= 8", 1)
//    test("(1 + { val x = 2; { val y = 3; x * { val x = 4; x + 1 } } } ) + 4", 15)
//    test("{ def f(a: Int, b: Int): Int = { a + b }; f(3, 4) }", 7)
//    test("{ var a = 4; a }", 4)
//    test("{ var a = 4; a = 6; a }", 6)
//    test("while (false) { 1 }", -1)
//    test("{ var a = 4; var i = 0; while (i <= 1) { i = i + 1; a = a + 1 }; a }", 6)
//    test("{ def f(a: Int, b: Int): Int = { a / b }; f(12, 4) }", 3)
    test("{ def fac(n: Int): Int = { def f(n: Int, acc: Int): Int = if (n == 0) acc else f(n - 1, n * acc); f(n, 1) }; fac(5) }", 120)
  }

  def test(prg: String, result: Int): Unit = {
    val hej = parse(prg)
    val hej2 = compile(hej)
    val hej3 = execute(hej2, Nil)
    assert(hej3 == result)
  }

}