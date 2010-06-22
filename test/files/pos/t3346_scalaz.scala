trait T[X]
trait U[X]
trait TC[M[_]]

object Test {
  def foo[M[_]: TC, A](ma: M[A]) = ()
  implicit val TCofT = new TC[T] {}
  implicit def any2T[A](a: A): T[A] = new T[A] {}
  implicit def any2U[A](a: A): U[A] = new U[A] {}
  // foo[T, Int](1)

  foo(1)        
  // <console>:13: error: type mismatch;
  //  found   : Int
  //  required: ?M[ ?A ]
  // Note that implicit conversions are not applicable because they are ambiguous:
  //  both method any2U in object $iw of type [A](a: A)U[A]
  //  and method any2T in object $iw of type [A](a: A)T[A]
  //  are possible conversion functions from Int to ?M[ ?A ]
  //        foo(1)
  //           ^
}