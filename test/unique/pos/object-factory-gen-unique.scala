/*-enable-unique*/
import scala.annotation.{unique, transient}

object HMap {

  def empty[A]: HMap[A]@unique = {
    val hm: HMap[A]@unique = new HMap[A]
    hm
  }

  def apply[A]: HMap[A]@unique = {
    val hm: HMap[A]@unique = empty[A]
    hm
  }
}

@transient class HMap[A] {
}
