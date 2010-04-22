/*-enable-unique*/
import scala.annotation.transient

class D {
  var f: C = _
}

@transient
class C {

  var f: C = _

  def m(x: D) {
    f = x.f
  }

}
