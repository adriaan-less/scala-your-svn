/*-enable-unique*/
import scala.annotation.{unique, uncheckedUnique}

class C {

  def consume(x: C @unique) {}

  def m(x: C) { // note: x is not unique
    consume((x: @uncheckedUnique))
  }

}
