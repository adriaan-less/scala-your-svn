/*-enable-unique*/
import scala.annotation.unique
import scala.annotation.Unique.share

class C {
  def m(x: C @unique) {
    m(x)
    share(x)
  }
}
