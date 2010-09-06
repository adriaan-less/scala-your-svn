class Base
class Sub extends Base
class Poly[T]

class A[T](val x: T) {
  def foo: T = x
  def poly[U <: T](y: U): T = y // bounds were messed up because TypeBounds (<: SubType) were erased to the higher bound
  
  class Inner[U](y: (T, U))
  
  def outerPoly[U]: Poly[U] = {
    class InnerMeth extends Poly[U]
    new InnerMeth
  }
}

class B[T, U <: A[T]]

object Test {
  val a = new A[String]("a")
  a.foo
  new a.Inner[Int](("1", 2))
  a.outerPoly[Base]

  val aa = new A[Base](new Base)
  val y = aa.poly[Sub](new Sub)
}