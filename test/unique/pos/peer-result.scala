/*-enable-unique*/
import scala.annotation.{unique, transient, peer}

class C {

  val f: C = null

  @transient def m(): C@peer(this) =
    this.f

  def consume(x: C @unique) {}

  @unique def m2() {
    consume(this.m())
  }

}
