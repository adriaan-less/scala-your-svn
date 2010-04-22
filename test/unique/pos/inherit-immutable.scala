/*-enable-unique*/
import scala.annotation.transient

import java.io.File

@transient
class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}
