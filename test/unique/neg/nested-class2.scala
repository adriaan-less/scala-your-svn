/*-enable-unique*/
import scala.annotation.{transient, unique, peer}

class C {

  class D {
    var f = C.this
    def m_D(x: C) {
      this.f = x
    }
  }

  def m(): D = {
    // error: unique value expected
    // reason: C.this, which is captured by D, is shared
    val d: D @unique = new D
    d
  }

}
