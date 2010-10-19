class A
class B extends A

// issues with overloading: this program prints 0327, whereas 1337 is expected
// the (B, B) overload is most specific in all cases below, but it is not selected in _a and _c
object Test extends Application {
  def foo_a(x: A, y: B) = print(0)
  val foo_a = new {
    def apply = (x: B, z: B) => print(1) // XXX: overloading not detected?
  }

  foo_a(new B, new B) // 0, expected: 1
// typer says: foo_a(new B(), new B());

  def foo_b(x: A, y: B) = print(0)
  val foo_b = new {
    def apply = (x: B, z: B) => print(3)
    def apply(x: B, y: A) = print(2)  // XXX: useless appendage, but changes result (causes overloading to be detected?)
  }

  foo_b(new B, new B) // 3, expected: 3
// typer says: foo_b.apply.apply(new B(), new B());

  def foo_c(x: A, y: B) = print(0)
  val foo_c = new {
    def apply = new { def apply = (x: B, z: B) => print(3) } // XXX: not considered to overload the other apply?
    def apply(x: B, y: A) = print(2) // useless appendage, but changes result
  }

  foo_c(new B, new B) // 2, expected: 3
// typer says: foo_c.apply(new B(), new B());

  def foo_d(x: A, y: B) = print(0)
  object foo_d {
    def apply(x: B, y: B) = print(7)
    // def apply(x: A, y: A) = print(2) // makes foo_d call below ambiguous, but shouldn't, the apply on the previous line is more specific
  }

  foo_d(new B, new B) // 7, expected: 7
// typer says: foo_d.apply(new B(), new B())
}
