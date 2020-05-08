package miniscala

import miniscala.Ast.MiniScalaError
import miniscala.parser.{Files, Parser}

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

      // compile to abstract machine code, if enabled
      if (Options.compile) {
        val outfile = (if (Options.file.endsWith(".s")) Options.file.substring(0, Options.file.length - 2) else Options.file) + ".sam"
        val bin = Compiler.compile(program)
        println(s"Executable (symbolic form): $bin")
        println(s"Writing executable to $outfile")
        Files.save(bin, outfile)
      }

      // load and execute abstract machine code, if enabled
      if (Options.machine) {
        val bin = Files.load(Options.file.substring(0, Options.file.length - 2) + ".sam")
        println(s"Executable (symbolic form): $bin")
        val initialEnv = AbstractMachine.makeInitialEnv(bin)
        val result = AbstractMachine.execute(bin, initialEnv)
        println(s"Output: $result")

      } else {

        // unparse the program, if enabled
        if (Options.unparse)
          println(Unparser.unparse(program))

        // type check the program, if enabled
        if (Options.types) {
          val initialTypeEnv = TypeChecker.makeInitialTypeEnv(program)
          TypeChecker.typeCheck(program, initialTypeEnv, Map())
        }

        // execute the program, if enabled
        if (Options.run) {
          val initialEnv = Interpreter.makeInitialEnv(program)
          val (result, _) = Interpreter.eval(program, initialEnv, Map(), Map())
          println(s"Output: ${Interpreter.valueToString(result)}")
        }
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
