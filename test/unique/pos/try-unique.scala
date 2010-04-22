/*-enable-unique*/

import java.io.{File, StringWriter, PrintWriter}

import scala.annotation.{unique, transient}
import scala.annotation.Unique.share

@transient
class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}

@transient case class RunTests(kind: String, files: List[File])
@transient case class Results(succ: Int, fail: Int, logs: List[LogFile], outdirs: List[File])
@transient case class LogContext(file: LogFile, writers: Option[(StringWriter, PrintWriter)])

@transient abstract class TestResult {
  def file: File
}
@transient case class Result(override val file: File, context: LogContext) extends TestResult
@transient case class Timeout(override val file: File) extends TestResult

trait FileManager {

  def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def getLogFile(dir: File, fileBase: String, kind: String): LogFile @unique = {
    val name = fileBase + "-" + kind + ".log"
    val file: LogFile @unique = new LogFile(dir, name)
    file
  }

  def getLogFile(file: File, kind: String): LogFile @unique = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    getLogFile(dir, fileBase, kind)
  }

}

object Test extends FileManager {

  def m(x: LogContext @unique) {

  }

  def main(args: Array[String]) {
    val file = new File("tmp")
    val logFile: LogFile @unique = getLogFile(file, "pos")

    val swr = new StringWriter
    val wr = new PrintWriter(swr)
    val ctx: LogContext @unique =
      LogContext(logFile, Some((swr, wr)))

    val context: LogContext @unique = try {
      LogContext(logFile, Some((swr, wr)))
    } catch {
      case t: Throwable =>
        //NestUI.verbose("while invoking compiler ("+files+"):")
        //NestUI.verbose("caught "+t)
        t.printStackTrace
        if (t.getCause != null)
          t.getCause.printStackTrace
        LogContext(null, None)
    }

    m(context)
  }
}

