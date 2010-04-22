/*-enable-unique*/
import scala.annotation.transient
import java.io.{File, StringWriter, PrintWriter}

// class is not transient, since it is not marked @transient
class D {
  var f: D = _
}

// should fail, since `D` is not transient
@transient case class RunTests(kind: String, d: D)

