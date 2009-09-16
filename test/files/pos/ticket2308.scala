object Test1 {
  trait T[A[_]]
  type T1 = T[A] forSome { type A[_] }
}
// 
// object Test2 {
//  trait Request[IN[_]]
//  trait T[A]
//  val request: Request[IN] forSome { type IN[_] } = new Request[T] {}
// }