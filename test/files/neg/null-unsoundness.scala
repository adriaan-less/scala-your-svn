class B
class C(x: String) extends B

class A {
  type A >: Null
  class D { type T >: C <: B }
  val x: D with A = null
  var y: x.T = new C("abc")
}
object Test extends A with Application {
  class C { type T = Int; val x = 1 }
  type A = C
  y = 42
}
  
