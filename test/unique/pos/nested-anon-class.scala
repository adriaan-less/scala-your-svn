/*-enable-unique*/
import scala.annotation.{transient, unique, peer}

class C {

  abstract class D {
    def m_D(x: C)
  }

  @transient
  def m(): D @peer(this) = {
    val d: D @unique = new D { // OK, has same capability as C.this
      var f = C.this
      def m_D(x: C) = {
        this.f = x
      }
    }
    d
  }
}
