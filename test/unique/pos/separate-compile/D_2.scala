/*-enable-unique*/
import scala.annotation.unique

class D {

  def m(): C @unique = {
    val c1: C @unique = new C
    val c2: C @unique = new C
    val res: C @unique = c1.m(c2)
    res
  }

}
