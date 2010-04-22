/*-enable-unique*/
import scala.annotation.transient

object O {
  var x: Option[E] = None
}

@transient
class E {
  def m(x: E) {
    O.x = Some(x)
  }
}
