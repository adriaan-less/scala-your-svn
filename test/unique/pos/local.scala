/*-enable-unique*/
import scala.annotation.transient

@transient
class C {

  var f = 0

  def m(x: C) {
    f = x.f
  }

}
