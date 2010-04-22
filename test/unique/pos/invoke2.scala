/*-enable-unique*/
// pos test
import scala.annotation.{transient, peer}

class C {

  var f: C = _

  def m(x: C @transient, y: C @peer(x)) {
    m(x.f, x)
    m(x, x.f)
  }

}
