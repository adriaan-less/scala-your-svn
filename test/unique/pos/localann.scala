/*-enable-unique*/
import scala.annotation.{transient, unique}

class D {
  var f: C = _

  def m(x: D @unique) {}
}

@transient
class C {

  var f: C = _

  def m(x: D) {
    f = x.f
  }

}
