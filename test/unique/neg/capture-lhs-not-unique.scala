/*-enable-unique*/
import scala.annotation.transient
import scala.annotation.Unique._

class C {

  def m(x: C @transient, y: C) {
    capture(y, x)
  }

}
