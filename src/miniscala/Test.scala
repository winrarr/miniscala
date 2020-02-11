package miniscala
import parser.Parser.parse

object Test {
  def main(args: Array[String]): Unit = {
    testOps()
    testUnparser()
  }

  def testOps(): Unit = {
    assert(Interpreter.eval(parse("32 - 7 * 3")) == 11)
    assert(Interpreter.eval(parse("5 * 6")) == 30)
    assert(Interpreter.eval(parse("25 % 11")) == 3)
  }

  def testUnparser(): Unit = {
    UnparserTestHelper("32 - 7 max 2 + 3")
    UnparserTestHelper("17 % 3 * 4 / 2")
    UnparserTestHelper("65 / 13 - 19 + 24")
    UnparserTestHelper("- 3")
  }

  def UnparserTestHelper(exp: String): Unit = {
    assert(exp == Unparser.unparse(parse(exp)))
  }
}
