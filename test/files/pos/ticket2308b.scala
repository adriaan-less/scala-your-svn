object Test2 {
 trait Request[IN[_]]
 trait T[A]
 val request: Request[IN] forSome { type IN[_] } = new Request[T] {}
}