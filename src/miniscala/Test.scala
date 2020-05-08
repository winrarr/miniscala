package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test112 {

  def main(args: Array[String]): Unit = {
    // interpreter
//    testValFail("""{ var z = null; z.f }""")
//    testVal("""{ class C() { }; { var x: C = null; x = new C() } }""".stripMargin, TupleVal(List[Val]()))
//    testVal("""{ class C() { }; { var x: C = new C(); x = null } }""".stripMargin, TupleVal(List[Val]()))
    testVal("""{ class C() { }; class D() { }; { var x: C = new C(); var z: C = new C(); x = null } }""".stripMargin, TupleVal(List[Val]()))

    // typechecker
    testTypeFail("{ class A() {}; new B() }")
    testTypeFail("{ val X = 42; new X() }")
    testTypeFail("{ class C(i: Int) { }; new C(1 - \"hello\") }")
    testTypeFail("{ class C(i: Int) { }; new C(1, 2) }")
    testTypeFail("42.f")
    testTypeFail("{ class C() { def f(x: Int): Int = 42 }; { val o = new C(); o.g } }")
    testTypeFail("{ class C(i) { } } ")
    testTypeFail("{ class C(i) { 1 - \"hello\" } } ")
    testTypeFail("{ class A() {}; class B() {}; { val x: A = new B() }}")
    testTypeFail("""
                   |    { class C() { val a: Boolean = false };
                   |      {
                   |        {
                   |          var x: C = new C();
                   |          class C() { val a: Int = 42 };
                   |          { val y: C = x }
                   |        }
                   |      }
                   |    }""".stripMargin)
    testTypeFail("""
                   |    { class C() { val a: Boolean = false };
                   |      {
                   |        {
                   |          var x: C = new C();
                   |          class C() { val a: Int = 42 };
                   |          { x = new C() }
                   |        }
                   |      }
                   |    }""".stripMargin)

    testType("""{ class A() { };
               |  class B() { var x: A = new A() } }""".stripMargin, unitType)
    testType("""{
               |  class Counter(init: Int) {
               |    var c: Int = init;
               |    def getValue(): Int = c;
               |    def inc(): Unit = { c = c + 1 }
               |  };
               |  {
               |    val x = new Counter(3);
               |    x.inc();
               |    x.inc();
               |    x.getValue()
               |  }
               |}""".stripMargin, IntType())
    testType("{ class A(x: Int) { val x: Int = x }; { def f(a: A): Int = a.x; f(new A(2)) } }", IntType())
    testType("{ class A() { }; class B(a: A) { }; new B(new A()) ; {} }", unitType)
  }

  def testVal(prg: String, value: Val, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()) = {
    val a = parse(prg)
    val (res, _) = eval(a, env, cenv, sto)
    assert(res == value)
  }

  def testValFail(prg: String, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()) = {
    try {
      eval(parse(prg), env, cenv, sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map(), ctenv: ClassTypeEnv = Map()) = {
    val a = parse(prg)
    assert(typeCheck(a, tenv, ctenv) == out)
  }

  def testTypeFail(prg: String) = {
    try {
      val a = parse(prg)
      typeCheck(a, Map(), Map())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}