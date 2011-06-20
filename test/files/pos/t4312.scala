class Bop[A[_]]

object Foo {
  private val stToSK = new Bop[ScopedTaskKey]
  private type ScopedTaskKey[T] = String
}