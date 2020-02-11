package miniscala

import miniscala.Ast.MiniScalaError
import miniscala.parser.Parser

object Main {

  /**
    * The main entry of MiniScala.
    */
  def main(args: Array[String]): Unit = {
    try {
      // read the command-line arguments
      Options.read(args)

      // parse the program
      val program = Parser.parse(Parser.readFile(Options.file))

      // unparse the program, if enabled
      if (Options.unparse)
        println(Unparser.unparse(program))

      // execute the program, if enabled
      if (Options.run) {
        val initialVarEnv = Interpreter.makeInitialVarEnv(program)
        val result = Interpreter.eval(program, initialVarEnv)
        println(s"Output: $result")
      }

    } catch { // report all errors to the console
      case e: Options.OptionsError =>
        println(e.getMessage)
        println(Options.usage)
      case e: MiniScalaError =>
        println(e.getMessage)
    }
  }
}
