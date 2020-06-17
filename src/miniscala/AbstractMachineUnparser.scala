package miniscala

import scala.collection.mutable
import AbstractMachine._

object AbstractMachineUnparser {

  def execute(ins: List[Instruction], initialEnv: List[Int]): String = {

    var code: List[Instruction] = ins // the program code to be executed
    val opstack = new mutable.Stack[String] // operand stack, contains values of sub-expressions
    val envstack = new mutable.Stack[Int] // environment stack, contains values of identifiers

    initialEnv.foreach(c => envstack.push(c))



    try {
      while (code.nonEmpty) {
        val inst = code.head
        code = code.tail
        inst match {
          case Const(c) =>
            opstack.push(c.toString)
          case Add =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 + $c2)")
          case Sub =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 - $c2)")
          case Mul =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 * $c2)")
          case Div =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 / $c2)")
          case Eq =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 == $c2)")
          case Lt =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 < $c2)")
          case Leq =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 <= $c2)")
          case And =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 & $c2)")
          case Or =>
            val c2 = opstack.pop()
            val c1 = opstack.pop()
            opstack.push(s"($c1 | $c2)")
          case Neg =>
            val c = opstack.pop()
            opstack.push(s"(-$c)")
          case Not =>
            val c = opstack.pop()
            opstack.push(s"(!$c)")
          case Branch(thencode, elsecode) =>
            opstack.push(s"if (${opstack.pop()}) ${execute(thencode, List())} else ${execute(elsecode, List())}")
          //          case EnterScope =>
          //            val c = opstack.pop()
          //            envstack.push(c)
          //          case ExitScope(num) =>
          //            for (_ <- 1 to num)
          //              envstack.pop()
          //          case Read(index) =>
          //            opstack.push(envstack(index))
        }
      }
      opstack.pop()
    } catch {
      case ex: Exception => throw new Error(ex)
    }
  }

}
