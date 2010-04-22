/*-enable-unique*/
import scala.annotation.{unique, transient}
import scala.annotation.Unique._

class C(name: String) {
  var f: C @unique = null

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

object Test {
  def main(args: Array[String]) {
    val c: C @unique = new C("default")
    val c2: C @unique = new C("default2")
    val old = swap(c.f, c2)
    println(old)
    val changed: C @unique = new C("changed")
    c.m(changed)
    val new1: C @unique = swap(c.f, null)
    new1.println()
  }
}
