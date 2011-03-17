// 1.scala
trait Derived[SA, This <: Derived[SA, This]] extends Base[SA, This] {
  class Meh extends super.Reversed with super.Transformed[SA] {}
  protected def newReversed: Meh = new Meh
}