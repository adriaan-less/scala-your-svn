abstract class B[T] {
  type T2 = T
  def m(t : T2): Any
}

object Test {
  val b : B[_] = error("")
  b.m("Hello") // should not typecheck, but b.T2 normalizes to Any!! 
  // thus, String <: b.T2 (because thirdTryRef in isSubtype normalizes)
  // val x: String = 1
}