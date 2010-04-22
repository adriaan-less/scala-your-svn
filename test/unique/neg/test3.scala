/*-enable-unique*/
import scala.annotation.unique

class Msg {}
class U {

  def send(m: Msg@unique) {}

  def foo(m: Msg) { // note: m is not unique
      send(m)
      send(m)
  }
}
