package test

// error: class Sub needs to be abstract, since method f in class Child of type 
// (x$1: test.TopLevelParent[T]#Inner)Unit is not defined
class Sub[A] extends Child[A] {
  def f(ic: test.TopLevelParent[A]#Inner): Unit = ()
}