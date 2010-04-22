/*-enable-unique*/

import java.io.{File, PrintWriter, FileWriter}

import scala.annotation.{unique, transient}
import scala.annotation.Unique.share

@transient
class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}

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

  def main(args: Array[String]) {
    val file = new File("tmp")
    val f: LogFile @unique = getLogFile(file, "pos")

    val e = new Exception("oops")

    // 1. expected type is not guarded
    // 2. expected param type is immutable
    // -> new instance is not guarded
    val fw = new FileWriter(f)

    val writer = new PrintWriter(fw, true)
    e.printStackTrace(writer)
    writer.close()

    // f should still be accessible
    share(f)
  }
}
