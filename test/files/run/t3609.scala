class A
class B extends A

object Test extends Application {
  def foo1(x: A, y: B) = print(1)
  val foo1 = new {
    //def apply(x: B, y: A) = print(3)
    def apply = (x: B, z: B) => print(4)
  }

  foo1(new B, new B) // 1

  // if we uncomment, will print 4.

  def foo2(x: A, y: B) = print(1)
  val foo2 = new {
    def apply(x: B, y: A) = print(3)
    def apply = new { def apply = (x: B, z: B) => print(4) }
  }

  foo2(new B, new B) // 3

  def foo3(x: A, y: B) = print(1)
  object foo3 {
    def apply(x: B, y: B) = print(3)
    //def apply(x: A, y: A) = print(5)
  }

  foo3(new B, new B) // 3
  // if we uncomment second apply then it doesn't compile. 
}

