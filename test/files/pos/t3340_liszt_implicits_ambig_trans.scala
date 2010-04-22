
object Test {
  class Foo(x: Meh) { def foo(a: String) = 1 }
  class Meh
  class A
  implicit object a extends A
  class B
  // implicit object b extends B
  
  implicit def mkFooA(x: Meh)(implicit w: A) : Foo = new Foo(x)
  implicit def mkFooB(x: Meh)(implicit w: B) : Foo = new Foo(x)

  (new Meh).foo("1")
}