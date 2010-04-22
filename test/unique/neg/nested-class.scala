/*-enable-unique*/
import scala.annotation.{transient, unique, peer}

class C {

  class D {
    var f = C.this
    def m_D(x: C) {
      this.f = x
    }
  }

  @transient
  def m(): D @unique = {
    val d: D @unique = new D // OK, has same capability as C.this
    d
    // error: expected unique result
  }

}
