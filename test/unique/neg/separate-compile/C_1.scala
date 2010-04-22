/*-enable-unique*/
import scala.annotation.{unique, transient, peer}
import scala.annotation.Unique._

class C {

  var f: C = _

  @transient
  def m(x: C @unique): C @peer(this) = {
    val xx: C @unique = capture(x, this)
    this.f = xx
    xx
  }

}
