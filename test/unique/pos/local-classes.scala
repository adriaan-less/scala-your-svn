/*-enable-unique*/
import scala.annotation.transient
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
