/*-enable-unique*/
// pos test
import scala.annotation.unique

class D {
  def consume(d: D@unique) { }

  def m1(d: D@unique) {
    d match {
      case x: D => consume(x)
    }
  }
}
