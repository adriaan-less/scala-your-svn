trait Try[T, F] {
  type Apply
}

object Try {
  // def implicitOrElse[T, F](implicit maybe: Try[T, F] = fail[T, F]): maybe.Apply = null.asInstanceOf[maybe.Apply]
  
  def fail[T, F] = new Try[T, F] {
    type Apply = F
  }

  implicit def ok[T, F](implicit w: T) = new Try[T, F] {
    type Apply = T
  }
}

//@implicitNotFound(msg="${T} must not be Nothing")
trait NotNothing[T]
object NotNothing {
  private trait Ok
  private trait IsNothing[T]
  private implicit object nuttin extends IsNothing[Nothing]

  implicit notNothing[T](implicit w: Try[IsNothing[T], Ok] = Try.fail[IsNothing[T], Ok], sub: w.Apply <:< Ok): NotNothing[T] = null.asInstanceOf[NotNothing[T]]
}
  
object Test {
  def iHateNothing[T: NotNothing](x: T): Int = 42  
  iHateNothing("a")
  iHateNothing(error("")) // must give error
}