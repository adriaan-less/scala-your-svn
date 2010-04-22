/*-enable-unique*/
import scala.annotation.unique

class D {

  def consume(x: C@unique) {}

  def m(): C @unique = {
    val c1: C @unique = new C
    val c2: C @unique = new C
    consume(c2)
    // error: cap of c2 not available!
    val res: C @unique = c1.m(c2)
    res
  }

}
