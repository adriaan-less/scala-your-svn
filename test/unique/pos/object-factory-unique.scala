/*-enable-unique*/
import scala.annotation.{unique, transient}

object HMap {

  def empty: HMap@unique = {
    val hm: HMap@unique = new HMap
    hm
  }

  def apply: HMap@unique = {
    val hm: HMap@unique = empty
    hm
  }
}

@transient class HMap {
}
