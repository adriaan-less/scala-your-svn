/*-enable-unique*/
import scala.annotation.{unique, transient}
import scala.annotation.Unique._

class C(name: String) {
  var f: C = null

  @transient
  def m(x: C @unique) {
    swap(this.f, x)
  }

  override def toString = name

  @transient
  def println() = {
    Console.println(name)
  }
}
