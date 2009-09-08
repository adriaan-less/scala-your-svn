object Test1 {
  trait T[A[_]]
  type T1 = T[E] forSome { type E[_] }
}