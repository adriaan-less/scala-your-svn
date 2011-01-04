object Test {
  trait WoeleWoele
  def m(v: V[T] forSome { type V[XXX] <: WoeleWoele; type T }) = 1
}