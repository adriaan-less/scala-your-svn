/*-enable-unique*/
import scala.annotation.unique

class C {
  var f: C @unique = null

  def m(x: C @unique) {
    // error: must use swap to access f!
    //swap(this, "f", x)
    f = x
  }

}
