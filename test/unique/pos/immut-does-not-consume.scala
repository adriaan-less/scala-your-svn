/*-enable-unique*/
import scala.annotation.{transient, unique}
import java.io.{File, StringWriter, PrintWriter}

@transient class LogFile(parent: File, child: String) extends File(parent, child) {
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

  def runTest(script: File => Unit) {
    //TODO: why do we have to make a ValDef file first?
    val file = new File("tmp")
    val logFile: LogFile @unique = getLogFile(file, "pos")
    val swr = new StringWriter
    val wr = new PrintWriter(swr)
    //script(logFile)
    val ctx: LogContext @unique =
      LogContext(logFile, Some((swr, wr)))
  }

}
