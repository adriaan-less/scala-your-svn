package scala.tools.nsc

import interpreter._
import java.io._

import scala.tools.nsc.{InterpreterResults => IR}
import scala.tools.nsc.interpreter._

// Classes to wrap up interpreter commands and their results
// You can add new commands by adding entries to val commands
// inside InterpreterLoop.
object InterpreterControl {
  // the default result means "keep running, and don't record that line"
  val defaultResult = Result(true, None)
  
  // a single interpreter command
  sealed abstract class Command extends Function1[List[String], Result] {
    val name: String
    val help: String
    def error(msg: String) = {
      println(":" + name + " " + msg + ".")
      Result(true, None)
    }
    def getHelp(): String = ":" + name + " " + help + "."
  }
  
  case class NoArgs(name: String, help: String, f: () => Result) extends Command {
    def apply(args: List[String]) = if (args.isEmpty) f() else error("accepts no arguments")
  }
  
  case class LineArg(name: String, help: String, f: (String) => Result) extends Command {
    def apply(args: List[String]) =
      if (args.size == 1) f(args.head)
      else error("requires a line of input")
  }

  case class OneArg(name: String, help: String, f: (String) => Result) extends Command {
    def apply(args: List[String]) =
      if (args.size == 1) f(args.head)
      else error("requires exactly one argument")
  }

  case class VarArgs(name: String, help: String, f: (List[String]) => Result) extends Command {
    def apply(args: List[String]) = f(args)
  }

  // the result of a single command
  case class Result(keepRunning: Boolean, lineToRecord: Option[String])
}
import InterpreterControl._

// import scala.concurrent.ops.defaultRunner

/** The 
 *  <a href="http://scala-lang.org/" target="_top">Scala</a>
 *  interactive shell.  It provides a read-eval-print loop around
 *  the Interpreter class.
 *  After instantiation, clients should call the <code>main()</code> method.
 *
 *  <p>If no in0 is specified, then input will come from the console, and
 *  the class will attempt to provide input editing feature such as
 *  input history.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author  Lex Spoon
 *  @version 1.2
 */
@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterLoop(in0: Option[BufferedReader], out: PrintWriter) extends ILoop(in0, out) {
  def this(in0: BufferedReader, out: PrintWriter) = this(Some(in0), out)
  def this() = this(None, new PrintWriter(scala.Console.out))
}
