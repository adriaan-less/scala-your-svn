/*-enable-unique*/
// pos test
import scala.annotation.{unique, peer}

class D {

  def m1(x: D @unique, y: D @peer(x)) {

  }

}
