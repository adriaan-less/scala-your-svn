/*-enable-unique*/
// neg test
import scala.annotation.{unique, transient}

class D {
  def consume(d: D@unique) { }

  def m1(d: D@transient) {
    consume(d)
    consume(d)
  }
}
