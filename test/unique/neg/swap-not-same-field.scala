/*-enable-unique*/
import scala.annotation.{unique, transient}
import scala.annotation.Unique._

class C {

  var f: C @unique = null
  var g: C @unique = null

  @transient
  def m() {
//    swap(this.f, null)
    val drop = { val tmp = this.f; this.g = null; tmp }
  }

}
