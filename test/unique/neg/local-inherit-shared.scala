/*-enable-unique*/
import scala.annotation.transient

class SuperFile(path: String, name: String)

@transient
class LogFile(parent: String, child: String) extends SuperFile(parent, child) {
  var toDelete = false
}
