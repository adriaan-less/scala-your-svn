/*-enable-unique*/
import scala.annotation.{transient, unique, peer}

class C {

  def consume(x: C @unique) {}

  @unique def m {
    consume(this)
  }

}
