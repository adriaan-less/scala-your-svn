trait Try[T, F, R] {
  def apply(f: F): R
}

object Try {
  def implicitOrElse[T, F, R](f: F)(implicit maybe: Try[T, F, R] = fail[T, F]): R = maybe(f)
  
  def fail[T, F] = new Try[T, F, F] {
    def apply(f: F) = f
  }

  implicit def ok[T, F](implicit w: T) = new Try[T, F, T] {
    def apply(f: F) = w
  }
}

object Test {
  sealed trait Ok
  sealed trait IsNothing[T]
  implicit object nuttin extends IsNothing[Nothing]
  
  def iHateNothing[T, R](x: T)(implicit w: Try[IsNothing[T], Ok, R] = Try.fail[IsNothing[T], Ok], sub: R <:< Ok): Int = 42
  
}

// trait InferNothing[+A]
// 
// trait NotNothing[A]
// object NotNothing {
//    implicit def definitelyNothing[A](implicit v: A =:= Nothing) = null.asInstanceOf[NotNothing[A] with InferNothing[A]]
//    implicit def maybeNothing[A] = null.asInstanceOf[NotNothing[A] with InferNothing[A]]
// }
// 
// object Test {
//   def dontWantNoNothing[T](x: T)(implicit w: NotNothing[T] with InferNothing[T]) = x
//   dontWantNoNothing("meh")
//   dontWantNoNothing(error(""))(NotNothing.definitelyNothing[Nothing])
// }