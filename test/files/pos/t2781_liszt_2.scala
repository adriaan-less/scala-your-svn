trait Test {
  type Rep[+T]

  implicit def unit[T](x : T) : Rep[T] = error("")
  // since Rep is covariant in T, Y will still be undetermined by the time we look for an implicit of type Int => Y
  implicit def convert[X,Y](x : Rep[X])(implicit c : X => Y): Rep[Y] = error("")
  
  class Vec[VT]

  class VecArithOps[T](x : Rep[Vec[T]]) {
    def /(y : Rep[T]) = error("")
  }
  implicit def VecArith[T](a : Rep[Vec[T]]) = new VecArithOps(a)

  val center : Rep[Vec[Float]] = error("")
  val s: Rep[Int] = error("")
  val x: Rep[Float] = center / (s) // BUG: need explicit convert call
}