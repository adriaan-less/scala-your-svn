/*-enable-unique*/
import scala.annotation.transient

class D {
  var f = 0
}

@transient
class C {

  var f = 0

  def m(x: D) {
    f = x.f
  }

}
