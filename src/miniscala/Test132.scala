package miniscala

import miniscala.AbstractMachine._
import miniscala.Compiler._
import miniscala.parser.Parser.parse

object Test132 {

  def main(args: Array[String]): Unit = {
    test("2 + 3", 5)
    test("-5", -5)
    test("5 - 2", 3)
    test("2 * 5", 10)
    test("10 / 2 * 3", 15)
    test("true & false", 0)
    test("false | true", 1)
    test("3 * 2 < 20", 1)
    test("3 == 3", 1)
    test("4 * 2 <= 8", 1)
    test("(1 + { val x = 2; { val y = 3; x * { val x = 4; x + 1 } } } ) + 4", 15)
  }

  def test(prg: String, result: Int) = {
    assert(execute(compile(parse(prg)), Nil) == result)
  }

}