import scala.annotation.{unique, transient}
import scala.annotation.Unique._

class C(name: String) {
  var f: C @unique = null

  @transient
  def m(x: C @unique, y: C @unique) {
    swap(x, y)
  }

  override def toString = name

  @transient
  def println() = {
    Console.println(name)
  }
}
