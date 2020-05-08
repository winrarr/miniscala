package miniscala.parser

import java.io._

import miniscala.AbstractMachine.Executable
import miniscala.Ast.MiniScalaError

import scala.util.parsing.input.NoPosition

object Files {

  def save(code: Executable, filename: String): Unit = {
    try {
      new File(filename).delete()
      val oos = new ObjectOutputStream(new FileOutputStream(filename))
      oos.writeObject(code)
      oos.close()
    } catch {
      case e: IOException =>
        throw new MiniScalaError(s"Unable to write file ${e.getMessage}", NoPosition)
    }
  }

  def load(filename: String): Executable = {
    try {
      val ois = new ObjectInputStream(new FileInputStream(filename))
      val code = ois.readObject.asInstanceOf[Executable]
      ois.close()
      code
    } catch {
      case e: IOException =>
        throw new MiniScalaError(s"Unable to read file ${e.getMessage}", NoPosition)
    }
  }
}
