/*-enable-unique*/
// pos test
import scala.annotation.{unique, peer}

class C {

  var f: C = _

  def m(x: C @unique, y: C @peer(x)) {
    m(x.f, x)
  }

}
