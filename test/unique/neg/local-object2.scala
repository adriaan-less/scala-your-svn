/*-enable-unique*/
import scala.annotation.transient

object O {
  var x: Option[E] = None
}

@transient
class E {
  def m2() {
    O.x = Some(this)
  }
}
