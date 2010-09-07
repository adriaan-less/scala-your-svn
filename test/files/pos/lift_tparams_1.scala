class Base
class Sub extends Base
class PolyT[T]

class AT[T](val x: T) {
  def foo: T = x
  def polyU[U <: T](y: U): T = y // bounds were messed up because TypeBounds (<: SubType) were erased to the higher bound
  
  class InnerU[U](y: (T, U))
  
  def outerPolyU[U]: PolyT[U] = {
    class ClassInMethod extends PolyT[U]
    def methodInMethod = 1
    object polyObjectInMethod extends PolyT[U]
    new ClassInMethod
  }
  
  object polyObjectInClass extends PolyT[T]
}

class BTU[T, U <: AT[T]]

object Test {
  val a = new AT[String]("a")
  a.foo
  new a.InnerU[Int](("1", 2))
  a.outerPolyU[Base]

  val aa = new AT[Base](new Base)
  val y = aa.polyU[Sub](new Sub)
}