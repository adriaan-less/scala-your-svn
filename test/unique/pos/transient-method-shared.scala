/*-enable-unique*/
import scala.annotation.{transient, peer}

class C {
  var f: Int = 0

  @transient def m(x: C @peer(this)) {
    x.f = 42
  }

  def m2() {
    val sharedC = new C
    // both this and sharedC are in shared region
    this.m(sharedC)
  }
}
