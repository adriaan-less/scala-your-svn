/*-enable-unique*/
import scala.annotation.unique
import scala.annotation.Unique._

class C {

  def m2(x: C @unique) {}

  def m(x: C @unique, y: C @unique) {
    m2(x)
    capture(x, y)
  }

}
