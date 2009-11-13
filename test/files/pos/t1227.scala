trait Foo[F[_]]

object Z {
  def unapply(f: Foo[T] forSome { type T[_] }) = Some(7)
}

object D {
  def f(x: Foo[List]) = x match {
    case Z(n) => error("todo")
  }
}
