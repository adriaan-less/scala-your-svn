//////////////////////////////////////////////////
trait A {
 def f[T[_]](x : T[Int]) : T[Any]
}

class B extends A {
 def f[T[+_]](x : T[Int]) : T[Any] = x
}

class P[Y](var y : Y)
//////////////////////////////////////////////////

//It accepted by the Scala 2.7.5 compiler without any errors. But is
//seems unsound, because then we can write (new B:A).f[P](new
//P[Int](1)), and nonvariant P[Int] is coerced to P[Any].
